port module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Date exposing (..)
import DatePicker exposing (DateEvent(..), defaultSettings)
import Debug
import Html exposing (Html, button, div, form, h1, h2, h3, h4, i, input, label, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (checked, class, colspan, id, placeholder, selected, step, type_, value)
import Html.Events exposing (onCheck, onClick, onInput)
import Http as Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as D exposing (hardcoded, optional, required)
import Json.Encode as Encode exposing (Value)
import Process
import RemoteData as RemoteData exposing (..)
import Select
import Simple.Fuzzy
import Task exposing (Task, perform, succeed)
import Time exposing (Weekday(..))



-- PORTS


port details : (Decode.Value -> msg) -> Sub msg


port fetchInvoiceDetails : List String -> Cmd msg


port navigateToSaleOrder : String -> Cmd msg



-- Model


type alias Model =
    { token : String
    , invoices : WebData InvoiceListResponse
    , invoiceDetails : Maybe (List InvoiceDetail)
    , orders : List Order
    , products : WebData ProductListResponse
    , contacts : WebData SearchContacts
    , restaurant : WebData Restaurant
    , salesOrder : WebData SalesOrder
    , contactSelect : Select.State
    , productSelect : Select.State
    , selectedContact : List SearchContact
    , orderLoading : Bool
    , orderDate : Maybe Date
    , orderFrequency : OrderFrequency
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


type alias QueuedOrder =
    { orderFreq : OrderFrequency }



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

        ( deliveryPicker, deliveryPickerCmd ) =
            DatePicker.init

        dateTask =
            Date.today
                |> Task.attempt InitializeDatePickers
    in
    ( { token = token
      , invoices = NotAsked
      , invoiceDetails = Nothing
      , orders = []
      , products = Loading
      , contacts = Loading
      , restaurant = NotAsked
      , salesOrder = NotAsked
      , contactSelect = Select.newState "contact-select"
      , selectedContact = [ SearchContact "28416000001299082" "R1" ]
      , productSelect = Select.newState "product-select"
      , orderLoading = False
      , orderDate = Nothing
      , orderFrequency = OneOff
      , orderDatePicker = orderPicker
      , deliveryDate = Nothing
      , deliveryDatePicker = deliveryPicker
      }
    , Cmd.batch
        [ fetchProducts token
        , fetchContacts token
        , fetchContactDetails token "28416000001299082"
        , Cmd.map ToOrderDatePicker orderPickerCmd
        , Cmd.map ToDeliveryDatePicker deliveryPickerCmd
        ]
    )



-- Msg


type Msg
    = InvoicesLoaded (WebData InvoiceListResponse)
    | InvoiceDetailsReceived (Result Decode.Error (List InvoiceDetail))
    | ProductsLoaded (WebData ProductListResponse)
    | ContactsLoaded (WebData SearchContacts)
    | RestaurantLoaded (WebData Restaurant)
    | AddOrder String Float
    | RemoveOrder String
    | UpdateOrder Int OrderSelector String
    | CreateSalesOrder
    | SalesOrderCreated (WebData SalesOrder)
    | OrdersCreated (Result Http.Error (List SalesOrder))
    | OnContactSelect (Maybe SearchContact)
    | ContactSelectMsg (Select.Msg SearchContact)
    | OnProductSelect (Maybe Product)
    | ProductSelectMsg (Select.Msg Product)
    | InitializeDatePickers (Result Never Date)
    | ToOrderDatePicker DatePicker.Msg
    | ToDeliveryDatePicker DatePicker.Msg
    | SetOrderFrequency OrderFrequency


type OrderFrequency
    = OneOff
    | Daily
    | Weekly
    | BiWeekly


type alias OrderDates =
    ( Date, Date )



-- Update


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
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

        SetOrderFrequency freq ->
            ( { model
                | orderFrequency = freq
              }
            , Cmd.none
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

        CreateSalesOrder ->
            let
                filterWeekend =
                    List.filter
                        (\d ->
                            Date.weekday d
                                /= Sat
                                && Date.weekday d
                                /= Sun
                        )

                dateRange orderFreq startDate =
                    case orderFreq of
                        Daily ->
                            Date.range Day 1 startDate (startDate |> Date.add Months 1)
                                |> filterWeekend

                        Weekly ->
                            Date.range Day 7 startDate (startDate |> Date.add Months 1)

                        BiWeekly ->
                            Date.range Day 14 startDate (startDate |> Date.add Months 1)

                        _ ->
                            []

                salesOrderTask restaurant orderDate deliveryDate =
                    fetchSalesOrderTemplateTask
                        model.token
                        |> Task.andThen
                            (\newTemplate ->
                                createDraftSalesOrderTask
                                    model.token
                                    restaurant
                                    newTemplate
                                    ( orderDate, deliveryDate )
                                    model.orders
                            )

                orderTasks orderFreq oDate startDate restaurant =
                    dateRange orderFreq startDate
                        |> List.map
                            (\date ->
                                salesOrderTask restaurant oDate date
                            )
                        |> Task.sequence
                        |> Task.attempt OrdersCreated

                createCmd =
                    case ( model.orderDate, model.deliveryDate, model.restaurant ) of
                        ( Just orderDate, Just deliveryDate, Success resto ) ->
                            case model.orderFrequency of
                                OneOff ->
                                    [ salesOrderTask resto orderDate deliveryDate ]
                                        |> Task.sequence
                                        |> Task.attempt OrdersCreated

                                Daily ->
                                    orderTasks Daily orderDate deliveryDate resto

                                Weekly ->
                                    orderTasks Weekly orderDate deliveryDate resto

                                BiWeekly ->
                                    orderTasks BiWeekly orderDate deliveryDate resto

                        ( _, _, _ ) ->
                            Cmd.none
            in
            ( {model | orderLoading = True }, createCmd )

        OrdersCreated (Ok salesOrders) ->
            let
                navigateCmd =
                    salesOrders
                        |> List.head
                        |> Maybe.map
                            (\so -> navigateToSaleOrder so.id)
                        |> Maybe.withDefault Cmd.none
            in
            ( { model | orderLoading = False }, navigateCmd )

        OrdersCreated (Err e) ->
            let
                _ =
                    Debug.log "err" e
            in
            ( model, Cmd.none )

        SalesOrderCreated data ->
            let
                orderCmd =
                    case ( data, model.orderFrequency ) of
                        ( Success salesOrder, OneOff ) ->
                            navigateToSaleOrder salesOrder.id

                        _ ->
                            Cmd.none
            in
            ( { model
                | salesOrder = NotAsked
                , orderDate = Nothing
                , deliveryDate = Nothing
                , orders = []
              }
            , orderCmd
            )

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
                , orders = []
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

        InitializeDatePickers (Ok today) ->
            let
                ( orderPicker, orderPickerCmd ) =
                    DatePicker.init

                deliveryPicker =
                    DatePicker.initFromDate <| Date.add Weeks 1 today
            in
            ( { model
                | orderDate = Just today
              }
            , Cmd.none
            )

        InitializeDatePickers _ ->
            ( model, Cmd.none )

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
        (case ( model.contacts, model.products ) of
            ( Success contacts, Success products ) ->
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
                , case (model.restaurant, model.orderLoading) of
                    (Success restaurant, False) ->
                        div [ class "main" ]
                            [ div [ class "invoice-wrapper" ]
                                (case model.invoiceDetails of
                                    Just invoiceDetails ->
                                        if List.isEmpty invoiceDetails then
                                            [ noInvoicesHtml
                                            ]

                                        else
                                            List.map invoiceItem invoiceDetails

                                    _ ->
                                        [ noInvoicesHtml ]
                                )
                            , div [ class "order-builder" ]
                                (orderBuilder restaurant model)
                            ]

                    (Loading, False) ->
                        loadingHtml "Restaurant"

                    (_, True) ->
                        loadingHtml "Placing Order(s)"

                    _ ->
                        div [ class "centered-header" ] []
                ]

            ( Loading, Loading ) ->
                [ loadingHtml "Contacts and Products" ]

            ( Loading, Success _ ) ->
                [ loadingHtml "" ]

            ( Success _, Loading ) ->
                [ loadingHtml "" ]

            _ ->
                [ div [ class "centered-header --error" ]
                    [ h1 [] [ text "There was an error :{" ] ]
                ]
        )


orderBuilder : Restaurant -> Model -> List (Html Msg)
orderBuilder restaurant model =
    let
        radio : OrderFrequency -> String -> Html Msg
        radio oFreq name =
            label
                [ class "order-radio" ]
                [ input [ type_ "radio", onCheck (\_ -> SetOrderFrequency oFreq), checked (model.orderFrequency == oFreq) ] []
                , text name
                ]
    in
    case model.salesOrder of
        NotAsked ->
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
                , div []
                    [ form [ class "order-frequencies" ]
                        [ radio OneOff "One Off"
                        , radio Daily "Daily"
                        , radio Weekly "Weekly"
                        , radio BiWeekly "BiWeekly"
                        ]
                    ]
                ]
            , orderTable model <| OrderTableContent model.orders
            , div [ class "submit-order" ]
                [ case ( model.orderDate, model.deliveryDate, List.length model.orders > 0 ) of
                    ( Just _, Just _, True ) ->
                        button [ onClick CreateSalesOrder ] [ text "Create Draft" ]

                    _ ->
                        div [] []
                ]
            ]

        Loading ->
            [ loadingHtml "New Order Draft" ]

        _ ->
            [ errorHtml "There was an error, refresh the page" ]


noInvoicesHtml : Html Msg
noInvoicesHtml =
    div [ class "centered-header --no-invoices" ]
        [ h2 [ class "no-results" ]
            [ text "No Prior Invoices" ]
        ]


loadingHtml : String -> Html Msg
loadingHtml message =
    div [ class "centered-header --loading" ]
        [ h1 []
            [ text <| "Loading - " ++ message
            , i [ class "far fa-hourglass" ] []
            ]
        ]


errorHtml : String -> Html Msg
errorHtml message =
    div [ class "centered-header --loading" ]
        [ h1 []
            [ text <| "Loading " ++ message
            , i [ class "far fa-times" ] []
            ]
        ]


invoiceItem : InvoiceDetail -> Html Msg
invoiceItem { lineitems, total, date } =
    let
        parsedDate =
            Date.fromIsoString date
                |> Result.toMaybe
                |> Maybe.map (\d -> Date.format "EEEE, d-MM-YY" d)
                |> Maybe.withDefault date
    in
    div []
        [ h4 [] [ text <| "Invoice Date - " ++ parsedDate ]
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
        totalWeight =
            List.foldl (\i acc -> i.quantity + acc) 0 items

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
            ((List.map lineItemRow items)
                ++ ([ tr [class "line-item"] 
                    [ td [] [text "total" ]
                    , td [] [ text <| String.fromFloat totalWeight ]
                    ]
                ])
            )
            
        ]


orderTable : Model -> OrderTable -> Html Msg
orderTable model (OrderTableContent orders) =
    let
        total =
            List.foldl (\o acc -> (o.product.price * o.quantity) + acc) 0 orders

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
    , invoices : List InvoiceListItem
    }


restaurantDecoder : Decoder Restaurant
restaurantDecoder =
    Decode.map5 Restaurant
        (Decode.at [ "contact", "contact_id" ] Decode.string)
        (Decode.at [ "contact", "name" ] Decode.string)
        (Decode.at [ "contact", "billing_address", "address_id" ] Decode.string)
        (Decode.at [ "contact", "shipping_address", "address_id" ] Decode.string)
        (Decode.at [ "invoices" ] (Decode.list invoiceListItemDecoder))



-- Sales Order Template


type alias SalesOrderTemplate =
    { templateId : String
    , nextNumber : String
    , prefixString : String
    , deliveryMethods : List DeliveryMethod
    }


salesOrderTemplateDecoder : Decoder SalesOrderTemplate
salesOrderTemplateDecoder =
    Decode.map4 SalesOrderTemplate
        (Decode.at [ "salesorder_settings", "default_template_id" ] Decode.string)
        (Decode.at [ "salesorder_settings", "next_number" ] Decode.string)
        (Decode.at [ "salesorder_settings", "prefix_string" ] Decode.string)
        (Decode.field "delivery_methods" (Decode.list deliveryMethodDecoder))


salesOrderCustomFieldEncoder : SalesOrderCustomField -> Encode.Value
salesOrderCustomFieldEncoder { label, value } =
    Encode.object
        [ ( "label", Encode.string label )
        , ( "value", Encode.string value )
        ]


saleOrderLineItemEncoder : Order -> Encode.Value
saleOrderLineItemEncoder { product, quantity, itemorder } =
    let
        ( noOfFish, port_ ) =
            ( SalesOrderCustomField "Port" "Plymouth"
            , SalesOrderCustomField "Number of Fish" "1"
            )
    in
    Encode.object
        [ ( "item_id", Encode.string product.id )
        , ( "item_order", Encode.string <| String.fromInt itemorder )

        -- , ( "rate", Encode.float product.price )
        , ( "name", Encode.string product.name )
        , ( "description", Encode.string "" )
        , ( "quantity", Encode.float quantity )
        , ( "discount", Encode.float 0 )
        , ( "tax_id", Encode.string "28416000000039007" )
        , ( "unit", Encode.string product.unit )
        , ( "tags", Encode.list Encode.string [] )
        , ( "item_custom_fields", Encode.list salesOrderCustomFieldEncoder [ port_ ] )
        ]


salesOrderEncoder :
    Restaurant
    -> SalesOrderTemplate
    -> ( Date, Date )
    -> List Order
    -> Encode.Value
salesOrderEncoder restaurant soTemplate dates orders =
    let
        { id, name, billingId, shippingId, invoices } =
            restaurant

        { templateId, nextNumber, prefixString, deliveryMethods } =
            soTemplate

        ( orderDate, shipmentDate ) =
            ( Tuple.first dates, Tuple.second dates )

        deliveryMethod =
            deliveryMethods
                |> List.head
                |> Maybe.map .method
                |> Maybe.withDefault "Same Day"
    in
    Encode.object
        [ ( "customer_id", Encode.string id )
        , ( "salesorder_number", Encode.string <| prefixString ++ nextNumber )
        , ( "date", Encode.string <| Date.format "YYYY-MM-dd" orderDate )
        , ( "shipment_date", Encode.string <| Date.format "YYYY-MM-dd" shipmentDate )
        , ( "exchange_rate", Encode.int 1 )
        , ( "delivery_method"
          , Encode.string deliveryMethod
          )
        , ( "line_items", Encode.list saleOrderLineItemEncoder orders )
        , ( "discount", Encode.int 0 )
        , ( "vat_treatment", Encode.string "uk" )
        , ( "template_id", Encode.string templateId )
        , ( "shipping_address_id", Encode.string shippingId )
        , ( "billing_address_id", Encode.string billingId )
        , ( "order_status", Encode.string "draft" )
        ]



-- SalesOrder


type alias SalesOrderCustomField =
    { label : String
    , value : String
    }


type alias SalesOrderLineItem =
    { itemOrder : Int
    , itemId : String
    , rate : Float
    , name : String
    , quantity : Float
    , discount : Float
    , taxId : String
    , unit : String
    , customFields : List SalesOrderCustomField
    }


type alias SalesOrder =
    { id : String
    }


salesOrderDecoder : Decoder SalesOrder
salesOrderDecoder =
    Decode.map SalesOrder
        (Decode.at [ "salesorder", "salesorder_id" ] Decode.string)



-- API


apiUrl : String -> String
apiUrl endpoint =
    "https://inventory.zoho.eu/api/v1/" ++ endpoint ++ "?organization_id=20062851724"


createDraftSalesOrder :
    String
    -> Restaurant
    -> SalesOrderTemplate
    -> ( Date, Date )
    -> List Order
    -> Cmd Msg
createDraftSalesOrder token restaurant template dates orders =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "JSONString" <|
                    Encode.encode 0 <|
                        salesOrderEncoder restaurant template dates orders
                ]
    in
    Http.request
        { method = "POST"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "salesorders"
        , body = body
        , expect = Http.expectJson (RemoteData.fromResult >> SalesOrderCreated) salesOrderDecoder
        , timeout = Nothing
        , tracker = Nothing
        }


createDraftSalesOrderTask :
    String
    -> Restaurant
    -> SalesOrderTemplate
    -> ( Date, Date )
    -> List Order
    -> Task Http.Error SalesOrder
createDraftSalesOrderTask token restaurant template dates orders =
    let
        body =
            Http.multipartBody
                [ Http.stringPart "JSONString" <|
                    Encode.encode 0 <|
                        salesOrderEncoder restaurant template dates orders
                ]
    in
    Http.task
        { method = "POST"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "salesorders"
        , body = body
        , resolver = Http.stringResolver <| handleJsonResponse salesOrderDecoder
        , timeout = Nothing
        }


getInvoicesForCompany : String -> String -> Cmd Msg
getInvoicesForCompany token searchTerm =
    Http.request
        { method = "GET"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "invoices"
                ++ "&search_text="
                ++ searchTerm
                ++ "&per_page=10&sort_column=created_time"
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


fetchSalesOrderTemplateTask : String -> Task Http.Error SalesOrderTemplate
fetchSalesOrderTemplateTask token =
    Http.task
        { method = "GET"
        , headers = [ Http.header "X-ZCSRF-TOKEN" token ]
        , url =
            apiUrl "salesorders/editpage/"
                ++ "&is_pre_tax=false"
        , body = Http.emptyBody
        , resolver = Http.stringResolver <| handleJsonResponse salesOrderTemplateDecoder
        , timeout = Nothing
        }



-- Util Functions


fireAction : (a -> msg) -> a -> Cmd msg
fireAction msgFn payload =
    perform msgFn (succeed payload)


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


handleJsonResponse : Decoder a -> Http.Response String -> Result Http.Error a
handleJsonResponse decoder response =
    case response of
        Http.BadUrl_ url ->
            Err (Http.BadUrl url)

        Http.Timeout_ ->
            Err Http.Timeout

        Http.BadStatus_ { statusCode } _ ->
            Err (Http.BadStatus statusCode)

        Http.NetworkError_ ->
            Err Http.NetworkError

        Http.GoodStatus_ _ body ->
            case Decode.decodeString decoder body of
                Err _ ->
                    Err (Http.BadBody body)

                Ok result ->
                    Ok result



-- Convenience Functions


sortSalesOrders : List Order -> List Order
sortSalesOrders orders =
    List.indexedMap (\i o -> { o | itemorder = i }) orders
