port module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug
import Html exposing (Html, button, div, h1, h2, h3, h4, i, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, id, placeholder, step, type_, value)
import Html.Events exposing (onClick, onInput)
import Http as Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as D exposing (hardcoded, optional, required)
import RemoteData as RemoteData exposing (..)
import Select
import Simple.Fuzzy
import Time exposing (Weekday(..))



-- PORTS FROM JAVASCRIPT
-- port onState : (Model -> msg) -> Sub msg


port details : (Decode.Value -> msg) -> Sub msg


port fetchInvoiceDetails : List String -> Cmd msg



-- Model


type alias Model =
    { token : String
    , searchTerm : String
    , invoices : WebData InvoiceListResponse
    , invoiceDetails : Maybe (List InvoiceDetail)
    , orders : List Order
    , products : WebData ProductListResponse
    , contacts : WebData SearchContacts
    , restaurant : WebData Restaurant
    , contactSelect : Select.State
    , productSelect : Select.State
    , selectedContact : List SearchContact
    , orderDate : Maybe Date
    , orderDatePicker : DatePicker.DatePicker
    , deliveryDate : Maybe Date
    , deliveryDatePicker : DatePicker.DatePicker
    }


type OrderSelector
    = Quantity
    | Rate
    | NumberOfFish


type alias Order =
    { product : Product
    , quantity : Float
    , itemorder : Int
    }


type alias SaleCustomField =
    { label : String
    , value : String
    }


type alias SaleLineItem =
    { itemOrder : Int
    , itemId : String
    , rate : Float
    , name : String
    , quantity : Float
    , discount : Float
    , taxId : String
    , unit : String
    , customFields : List SaleCustomField
    }


type alias SalesOrder =
    { customerid : String
    , customerName : String
    }



-- Configs


productSelectConfig : Select.Config Msg Product
productSelectConfig =
    Select.newConfig
        { onSelect = OnProductSelect
        , toLabel = .name
        , filter = filter 3 .name
        }
        |> Select.withPrompt "Select a product"


contactSelectConfig : Select.Config Msg SearchContact
contactSelectConfig =
    Select.newConfig
        { onSelect = OnContactSelect
        , toLabel = .name
        , filter = filter 1 .name
        }
        |> Select.withPrompt "Search for a restaurant"


orderPickerSettings : DatePicker.Settings
orderPickerSettings =
    { defaultSettings
        | placeholder = "Order Date"
        , firstDayOfWeek = Mon
    }


deliveryPickerSettings : DatePicker.Settings
deliveryPickerSettings =
    { defaultSettings
        | placeholder = "Delivery Date"
        , firstDayOfWeek = Mon
    }



-- Init


init : String -> ( Model, Cmd Msg )
init token =
    let
        ( orderPicker, orderPickerCmd ) =
            DatePicker.init
    in
    ( { token = token
      , searchTerm = "Leroy"
      , invoices = NotAsked
      , invoiceDetails = Nothing
      , orders = []
      , products = Loading
      , contacts = Loading
      , restaurant = NotAsked
      , contactSelect = Select.newState ""
      , selectedContact = []
      , productSelect = Select.newState ""
      , orderDate = Nothing
      , orderDatePicker = orderPicker
      , deliveryDate = Nothing
      , deliveryDatePicker = orderPicker
      }
    , Cmd.batch
        [ fetchProducts token
        , fetchContacts token
        , Cmd.map ToOrderDatePicker orderPickerCmd
        , Cmd.map ToDeliveryDatePicker orderPickerCmd
        ]
    )



-- Msg


type Msg
    = SearchInputChanged String
    | InvoicesLoaded (WebData InvoiceListResponse)
    | InvoiceDetailsReceived (Result Decode.Error (List InvoiceDetail))
    | ProductsLoaded (WebData ProductListResponse)
    | ContactsLoaded (WebData SearchContacts)
    | RestaurantLoaded (WebData Restaurant)
    | AddOrder String Float
    | RemoveOrder String
    | UpdateOrder Int OrderSelector String
    | OnContactSelect (Maybe SearchContact)
    | ContactSelectMsg (Select.Msg SearchContact)
    | OnProductSelect (Maybe Product)
    | ProductSelectMsg (Select.Msg Product)
    | ToOrderDatePicker DatePicker.Msg
    | ToDeliveryDatePicker DatePicker.Msg



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInputChanged new ->
            ( { model | searchTerm = new }, Cmd.none )

        InvoicesLoaded invoices ->
            let
                fetchCmd =
                    case invoices of
                        Success data ->
                            if List.length data.invoices > 0 then
                                List.map .id data.invoices
                                    |> fetchInvoiceDetails

                            else
                                Cmd.none

                        _ ->
                            Cmd.none
            in
            ( { model
                | invoices = invoices
              }
            , fetchCmd
            )

        InvoiceDetailsReceived results ->
            ( { model | invoiceDetails = Result.toMaybe results }, Cmd.none )

        ProductsLoaded products ->
            ( { model | products = products }, Cmd.none )

        ContactsLoaded contacts ->
            ( { model | contacts = contacts }, Cmd.none )

        RestaurantLoaded restaurant ->
            let
                invoiceDetailsCmd =
                    case restaurant of
                        Success r ->
                            getInvoicesForCompany model.token r.name

                        _ ->
                            Cmd.none
            in
            ( { model | restaurant = restaurant }, invoiceDetailsCmd )

        AddOrder productName quantity ->
            let
                maybeOrder =
                    case model.products of
                        Success data ->
                            List.filter (\p -> p.name == productName) data.results
                                |> List.head
                                |> Maybe.map
                                    (\p ->
                                        Order p quantity (List.length model.orders)
                                    )

                        _ ->
                            Nothing

                orders =
                    case
                        ( model.orders
                            |> List.any (\o -> o.product.name == productName)
                            |> not
                        , maybeOrder
                        )
                    of
                        ( True, Just o ) ->
                            model.orders ++ [ o ]

                        _ ->
                            model.orders
            in
            ( { model | orders = sortSalesOrders orders }, Cmd.none )

        RemoveOrder name ->
            ( { model | orders = List.filter (\o -> o.product.name /= name) model.orders }, Cmd.none )

        UpdateOrder itemorder type_ val ->
            let
                quantity =
                    String.toFloat val
                        |> Maybe.withDefault 0

                orders =
                    model.orders
                        |> updateAt itemorder (\o -> { o | quantity = quantity })
            in
            ( { model | orders = sortSalesOrders orders }, Cmd.none )

        ContactSelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update contactSelectConfig subMsg model.contactSelect
            in
            ( { model | contactSelect = updated }, cmd )

        OnContactSelect maybeContact ->
            let
                ( contact, selectContactCmd ) =
                    case maybeContact of
                        Just c ->
                            ( c, fetchContactDetails model.token c.id )

                        Nothing ->
                            ( SearchContact "" "", Cmd.none )
            in
            ( { model
                | selectedContact = [ contact ]
                , restaurant = Loading
              }
            , selectContactCmd
            )

        ProductSelectMsg subMsg ->
            let
                ( updated, cmd ) =
                    Select.update productSelectConfig subMsg model.productSelect
            in
            ( { model | productSelect = updated }, cmd )

        OnProductSelect maybeProduct ->
            let
                selectedProducts =
                    case maybeProduct of
                        Just product ->
                            model.orders ++ [ Order product 1 (List.length model.orders) ]

                        Nothing ->
                            model.orders
            in
            ( { model | orders = selectedProducts }, Cmd.none )

        ToOrderDatePicker subMsg ->
            let
                ( updatedPicker, pickerCmd ) =
                    DatePicker.update orderPickerSettings subMsg model.orderDatePicker

                selectedDate =
                    case pickerCmd of
                        Picked pickedDate ->
                            Just pickedDate

                        _ ->
                            model.orderDate
            in
            ( { model
                | orderDate = selectedDate
                , orderDatePicker = updatedPicker
              }
            , Cmd.none
            )

        ToDeliveryDatePicker subMsg ->
            let
                ( updatedPicker, pickerCmd ) =
                    DatePicker.update deliveryPickerSettings subMsg model.deliveryDatePicker

                selectedDate =
                    case pickerCmd of
                        Picked pickedDate ->
                            Just pickedDate

                        _ ->
                            model.orderDate
            in
            ( { model
                | deliveryDate = selectedDate
                , deliveryDatePicker = updatedPicker
              }
            , Cmd.none
            )


view : Model -> Html Msg
view model =
    case ( model.contacts, model.products ) of
        ( Success contacts, Success products ) ->
            div
                [ class "content"
                , id "zoho-helper"
                ]
                [ div [ class "header" ]
                    [ div [] [ h3 [] [ text "Zoho Helper" ] ]
                    , div [ class "search-controls" ]
                        [ Select.view
                            contactSelectConfig
                            model.contactSelect
                            contacts.contacts
                            model.selectedContact
                            |> Html.map ContactSelectMsg
                        ]
                    ]
                , case ( model.invoiceDetails, model.restaurant ) of
                    ( Just invoiceDetails, Success restaurant ) ->
                        div [ class "main" ]
                            [ div [ class "invoice-wrapper" ]
                                (if List.isEmpty invoiceDetails then
                                    [ div [ class "centered-header --no-invoices" ]
                                        [ h2 [ class "no-results" ]
                                            [ text "No Prior Invoices" ]
                                        ]
                                    ]

                                 else
                                    List.map invoiceItem invoiceDetails
                                )
                            , div [ class "order-builder" ]
                                [ h3 [] [ text <| "Order Builder - " ++ restaurant.name ]
                                , div [ class "date-pickers" ]
                                    [ div []
                                        [ h4 [] [ text "Order Date" ]
                                        , DatePicker.view model.orderDate orderPickerSettings model.orderDatePicker
                                            |> Html.map ToOrderDatePicker
                                        ]
                                    , div []
                                        [ h4 [] [ text "Delivery Date" ]
                                        , DatePicker.view model.deliveryDate deliveryPickerSettings model.deliveryDatePicker
                                            |> Html.map ToDeliveryDatePicker
                                        ]
                                    ]
                                , orderTable model <| OrderTableContent model.orders
                                ]
                            ]

                    ( _, Loading ) ->
                        loadingHtml

                    _ ->
                        div [ class "centered-header" ] []
                ]

        ( Loading, Loading ) ->
            loadingHtml

        ( Loading, Success _ ) ->
            loadingHtml

        ( Success _, Loading ) ->
            loadingHtml

        _ ->
            div [ class "centered-header --error" ]
                [ h1 [] [ text "There was an error :{" ] ]


loadingHtml : Html Msg
loadingHtml =
    div [ class "centered-header --loading" ]
        [ h1 []
            [ text "Loading"
            , i [ class "far fa-hourglass" ] []
            ]
        ]


invoiceItem : InvoiceDetail -> Html Msg
invoiceItem { lineitems, total, date } =
    div []
        [ h4 [] [ text <| "Invoice Date - " ++ date ]
        , invoiceTable (InvoiceTableContent total lineitems)
        ]


type alias InvoiceTotal =
    Float


type InvoiceTable
    = InvoiceTableContent InvoiceTotal (List InvoiceLineItem)


type OrderTable
    = OrderTableContent (List Order)


invoiceTable : InvoiceTable -> Html Msg
invoiceTable (InvoiceTableContent total items) =
    let
        lineItemRow { name, quantity, itemtotal } =
            tr [ class "line-item" ]
                [ td [] [ text name ]
                , td []
                    [ text <|
                        String.fromFloat quantity
                    ]
                , td [] [ text <| String.fromFloat itemtotal ]
                , td [ onClick (AddOrder name quantity) ]
                    [ i [ class "fas fa-plus" ] [] ]
                ]
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text <| "Product" ]
                , th [] [ text "Quantity" ]
                , th []
                    [ text <| "Total: " ++ String.fromFloat total
                    ]
                , th [] [ text "Add" ]
                ]
            ]
        , tbody [ class "--invoices" ]
            (List.map lineItemRow items)
        ]


orderTable : Model -> OrderTable -> Html Msg
orderTable model (OrderTableContent orders) =
    let
        total =
            List.foldl (\o acc -> o.product.price + acc) 0 orders

        lineItemRow { name, quantity, price, unit, itemorder } =
            tr [ class "line-item" ]
                [ td [] [ text name ]
                , td []
                    [ input
                        [ type_ "number"
                        , step "0.5"
                        , onInput (UpdateOrder itemorder Quantity)
                        , value <| String.fromFloat quantity
                        ]
                        []
                    ]
                , td [] [ text <| String.fromFloat (price * quantity) ]
                , td [] [ text unit ]
                , td [ onClick (RemoveOrder name) ]
                    [ i [ class "fas fa-times-circle" ] [] ]
                ]

        orderRows =
            List.map
                (\{ product, quantity, itemorder } ->
                    lineItemRow
                        { name = product.name
                        , quantity = quantity
                        , price = product.price
                        , unit = product.unit
                        , itemorder = itemorder
                        }
                )
                orders

        extractedProducts =
            case model.products of
                Success data ->
                    data.results

                _ ->
                    []
    in
    table []
        [ thead []
            [ tr []
                [ th [] [ text <| "Product" ]
                , th [] [ text "Quantity" ]
                , th []
                    [ text <| "Total: " ++ String.fromFloat total
                    ]
                , th [] [ text "Unit" ]
                , th [] [ text "Remove" ]
                ]
            ]
        , tbody [ class "--orders" ]
            (orderRows
                ++ [ Html.map ProductSelectMsg <|
                        tr [ class "line-item" ]
                            [ td [ class "autocompleter", colspan 5 ]
                                [ Select.view
                                    productSelectConfig
                                    model.productSelect
                                    extractedProducts
                                    []
                                ]
                            ]
                   ]
            )
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    details (invoiceDetailsResponseDecoder >> InvoiceDetailsReceived)


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Data Models
-- Invoice List Item


type alias InvoiceListItem =
    { id : String
    , customerId : String
    , customerName : String
    , date : String
    }


invoiceListItemDecoder : Decoder InvoiceListItem
invoiceListItemDecoder =
    Decode.succeed InvoiceListItem
        |> D.required "invoice_id" Decode.string
        |> D.required "customer_id" Decode.string
        |> D.required "customer_name" Decode.string
        |> D.required "date" Decode.string



-- Invoice List Response


type alias InvoiceListResponse =
    { invoices : List InvoiceListItem }


invoiceListResponseDecoder : Decoder InvoiceListResponse
invoiceListResponseDecoder =
    Decode.succeed InvoiceListResponse
        |> D.required "invoices" (Decode.list invoiceListItemDecoder)



-- Invoice Line Item


type alias InvoiceLineItem =
    { id : String
    , name : String
    , quantity : Float
    , itemtotal : Float
    }


invoiceLineItemDecoder : Decoder InvoiceLineItem
invoiceLineItemDecoder =
    Decode.succeed InvoiceLineItem
        |> D.required "line_item_id" Decode.string
        |> D.required "name" Decode.string
        |> D.required "quantity" Decode.float
        |> D.required "item_total" Decode.float



-- Invoice Detail


type alias InvoiceDetail =
    { id : String
    , lineitems : List InvoiceLineItem
    , date : String
    , total : Float
    }


invoiceDetailDecoder : Decoder InvoiceDetail
invoiceDetailDecoder =
    Decode.succeed InvoiceDetail
        |> D.required "invoice_id" Decode.string
        |> D.required "line_items" (Decode.list invoiceLineItemDecoder)
        |> D.required "date" Decode.string
        |> D.required "total" Decode.float



-- Invoice Detail Response


type alias InvoiceDetailResponse =
    { invoice : InvoiceDetail }


invoiceDetailsResponseDecoder : Decode.Value -> Result Decode.Error (List InvoiceDetail)
invoiceDetailsResponseDecoder =
    Decode.decodeValue
        (Decode.field "invoices" (Decode.list invoiceDetailDecoder))



-- Product


type alias Product =
    { id : String
    , name : String
    , price : Float
    , unit : String
    }


productDecoder : Decoder Product
productDecoder =
    Decode.succeed Product
        |> D.required "id" Decode.string
        |> D.required "text" Decode.string
        |> D.required "rate" Decode.float
        |> D.required "unit" Decode.string



-- Product List Response


type alias ProductListResponse =
    { results : List Product
    }


productListResponseDecoder : Decoder ProductListResponse
productListResponseDecoder =
    Decode.succeed ProductListResponse
        |> D.required "results" (Decode.list productDecoder)



-- Contact


type alias SearchContact =
    { id : String
    , name : String
    }


type alias SearchContacts =
    { contacts : List SearchContact }


searchContactsDecoder : Decoder SearchContacts
searchContactsDecoder =
    Decode.succeed SearchContacts
        |> D.required "results"
            (Decode.list
                (Decode.succeed SearchContact
                    |> D.required "id" Decode.string
                    |> D.required "text" Decode.string
                )
            )


type alias DeliveryMethod =
    { id : String
    , method : String
    }


deliveryMethodDecoder : Decoder DeliveryMethod
deliveryMethodDecoder =
    Decode.succeed DeliveryMethod
        |> D.required "delivery_method_id" Decode.string
        |> D.required "delivery_method" Decode.string


type alias Restaurant =
    { id : String
    , name : String
    , billingId : String
    , shippingId : String
    , deliveryMethods : List DeliveryMethod
    , invoices : List InvoiceListItem
    }


restaurantDecoder : Decoder Restaurant
restaurantDecoder =
    Decode.map6 Restaurant
        (Decode.at [ "contact", "contact_id" ] Decode.string)
        (Decode.at [ "contact", "name" ] Decode.string)
        (Decode.at [ "contact", "billing_address", "address_id" ] Decode.string)
        (Decode.at [ "contact", "shipping_address", "address_id" ] Decode.string)
        (Decode.at [ "delivery_methods" ] (Decode.list deliveryMethodDecoder))
        (Decode.at [ "invoices" ] (Decode.list invoiceListItemDecoder))



-- API


apiUrl : String -> String
apiUrl endpoint =
    "https://inventory.zoho.eu/api/v1/" ++ endpoint ++ "?organization_id=20062851724"


getInvoicesForCompany : String -> String -> Cmd Msg
getInvoicesForCompany token searchTerm =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "invoices"
                ++ "&search_text="
                ++ searchTerm
                ++ "&per_page=6&sort_column=created_time"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> InvoicesLoaded) invoiceListResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchProducts : String -> Cmd Msg
fetchProducts token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "autocomplete/product"
                ++ "&search_text=&item_type=sales"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> ProductsLoaded) productListResponseDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchContacts : String -> Cmd Msg
fetchContacts token =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "autocomplete/contact"
                ++ "&search_text=&contact_type=customer"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> ContactsLoaded) searchContactsDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


fetchContactDetails : String -> String -> Cmd Msg
fetchContactDetails token id =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "salesorders/editpage/fromcontacts"
                ++ "&contact_id="
                ++ id
                ++ "&disabled_settings=true"
        , body = Http.emptyBody
        , expect = Http.expectJson (RemoteData.fromResult >> RestaurantLoaded) restaurantDecoder
        , timeout = Nothing
        , tracker = Nothing
        }



-- Util Functions


filter : Int -> (a -> String) -> String -> List a -> Maybe (List a)
filter minChars toLabel query items =
    if String.length query < minChars then
        Nothing

    else
        items
            |> Simple.Fuzzy.filter toLabel query
            |> Just


pushIf : Bool -> a -> List a -> List a
pushIf pred thing list =
    if pred then
        list ++ [ thing ]

    else
        list


updateAt : Int -> (a -> a) -> List a -> List a
updateAt index fn list =
    if index < 0 then
        list

    else
        let
            head =
                List.take index list

            tail =
                List.drop index list
        in
        case tail of
            x :: xs ->
                head ++ fn x :: xs

            _ ->
                list



-- Order Selector
-- type SelectorMsg
--     = Toggle SelectorMode
--     | UpdateValue String
-- type SelectorMode
--     = Edit
--     | Display
-- type alias OrderSelector =
--     { value : Maybe Float
--     , mode : SelectorMode
--     }
-- selectorUpdate : OrderSelector -> SelectorMsg -> ( OrderSelector, Cmd SelectorMsg )
-- selectorUpdate model msg =
--     case msg of
--         Toggle mode ->
--             ( { model | mode = mode }, Cmd.none )
--         UpdateValue val ->
--             ( { model | value = String.toFloat val }, Cmd.none )
-- selectorView : OrderSelector -> Html SelectorMsg
-- selectorView { value, mode } =
--     div []
--         [ case mode of
--             Display ->
--                 text <| String.fromFloat <| Maybe.withDefault 0 <| value
--             Edit ->
--                 input [ type_ "number", step "0.01", onInput UpdateValue ] []
--         ]
-- Convenience Functions


sortSalesOrders : List Order -> List Order
sortSalesOrders orders =
    List.indexedMap (\i o -> { o | itemorder = i }) orders
