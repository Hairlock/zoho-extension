port module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Date exposing (Date)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug
import Html exposing (Html, button, div, h1, h2, h3, h4, i, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, colspan, id, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http as Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as D exposing (hardcoded, optional, required)
import RemoteData as RemoteData exposing (..)
import Select
import Simple.Fuzzy



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
    , productSelect : Select.State
    , selectedProducts : List Product
    , salesOrder : Maybe SalesOrder
    , orderDate : Maybe Date
    , orderDatePicker : DatePicker.DatePicker
    , deliveryDate : Maybe Date
    , deliveryDatePicker : DatePicker.DatePicker
    }


type alias Order =
    { product : Product
    , quantity : Float
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


orderPickerSettings : DatePicker.Settings
orderPickerSettings =
    { defaultSettings | placeholder = "Order Date" }


deliveryPickerSettings : DatePicker.Settings
deliveryPickerSettings =
    { defaultSettings | placeholder = "Delivery Date" }



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
      , products = NotAsked
      , productSelect = Select.newState ""
      , selectedProducts = []
      , salesOrder = Nothing
      , orderDate = Nothing
      , orderDatePicker = orderPicker
      , deliveryDate = Nothing
      , deliveryDatePicker = orderPicker
      }
    , Cmd.batch
        [ fetchProducts token
        , Cmd.map ToOrderDatePicker orderPickerCmd
        , Cmd.map ToDeliveryDatePicker orderPickerCmd
        ]
    )



-- Msg


type Msg
    = SearchInputChanged String
    | LoadInvoices
    | InvoicesLoaded (WebData InvoiceListResponse)
    | InvoiceDetailsReceived (Result Decode.Error (List InvoiceDetail))
    | ProductsLoaded (WebData ProductListResponse)
    | AddOrder String Float
    | RemoveOrder String
    | OnProductSelect (Maybe Product)
    | ProductSelectMsg (Select.Msg Product)
    | ToOrderDatePicker DatePicker.Msg
    | ToDeliveryDatePicker DatePicker.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInputChanged new ->
            ( { model | searchTerm = new }, Cmd.none )

        LoadInvoices ->
            ( model, getInvoicesForCompany model )

        InvoicesLoaded invoices ->
            let
                ( fetchCmd, salesOrder ) =
                    case invoices of
                        Success data ->
                            if List.length data.invoices > 0 then
                                ( List.map .id data.invoices
                                    |> fetchInvoiceDetails
                                , data.invoices
                                    |> List.head
                                    |> Maybe.map (\c -> ( c.customerId, c.customerName ))
                                    |> Maybe.andThen (\( id, name ) -> Just <| SalesOrder id name)
                                )

                            else
                                ( Cmd.none, Nothing )

                        _ ->
                            ( Cmd.none, Nothing )
            in
            ( { model
                | invoices = invoices
                , salesOrder = salesOrder
              }
            , fetchCmd
            )

        InvoiceDetailsReceived results ->
            ( { model | invoiceDetails = Result.toMaybe results }, Cmd.none )

        ProductsLoaded products ->
            ( { model | products = products }, Cmd.none )

        AddOrder productName quantity ->
            let
                maybeOrder =
                    case model.products of
                        Success data ->
                            List.filter (\p -> p.name == productName) data.results
                                |> List.head
                                |> Maybe.map
                                    (\p ->
                                        Order p quantity
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
                            o :: model.orders

                        _ ->
                            model.orders
            in
            ( { model | orders = orders }, Cmd.none )

        RemoveOrder name ->
            ( { model | orders = List.filter (\o -> o.product.name /= name) model.orders }, Cmd.none )

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
                            model.orders ++ [ Order product 1 ]

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
    div
        [ class "content"
        , id "zoho-helper"
        ]
        [ div [ class "header" ]
            [ div [] [ h3 [] [ text "Zoho Helper" ] ]
            , div [ class "search-controls" ]
                [ input [ onInput SearchInputChanged, placeholder "Company Name", value model.searchTerm ] []
                , button [ onClick LoadInvoices ] [ text "Search" ]
                ]
            ]
        , case ( model.invoiceDetails, model.salesOrder ) of
            ( Just invoiceDetails, Just salesOrder ) ->
                div [ class "main" ]
                    [ div [ class "invoice-wrapper" ]
                        (List.map invoiceItem invoiceDetails)
                    , div [ class "order-builder" ]
                        ([ h3 [] [ text <| "Order Builder - " ++ salesOrder.customerName ]
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
                         ]
                            |> pushIf (List.length model.orders > 0)
                                (orderTable model <| OrderTableContent model.orders)
                        )
                    ]

            _ ->
                div [] []
        ]


invoiceItem : InvoiceDetail -> Html Msg
invoiceItem { lineitems, total } =
    div []
        [ invoiceTable (InvoiceTableContent total lineitems)
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
                , td [] [ text <| String.fromFloat quantity ]
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

        lineItemRow { name, quantity, itemtotal, unit } =
            tr [ class "line-item" ]
                [ td [] [ text name ]
                , td [] [ text <| String.fromFloat quantity ]
                , td [] [ text <| String.fromFloat itemtotal ]
                , td [] [ text unit ]
                , td [ onClick (RemoveOrder name) ]
                    [ i [ class "fas fa-times-circle" ] [] ]
                ]

        orderRows =
            List.map
                (\{ product, quantity } ->
                    lineItemRow
                        { name = product.name
                        , quantity = quantity
                        , itemtotal = product.price
                        , unit = product.unit
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
                                    model.selectedProducts
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



-- API


apiUrl : String -> String
apiUrl endpoint =
    "https://inventory.zoho.eu/api/v1/" ++ endpoint ++ "?organization_id=20062851724"


getInvoicesForCompany : Model -> Cmd Msg
getInvoicesForCompany { token, searchTerm } =
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
