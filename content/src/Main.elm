port module Main exposing (Msg(..), init, main, subscriptions, update, view)

import Browser
import Debug
import Html exposing (Html, button, div, h1, h2, h3, h4, i, input, span, table, tbody, td, text, th, thead, tr)
import Html.Attributes exposing (class, placeholder, value)
import Html.Events exposing (onClick, onInput)
import Http as Http
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline as D exposing (hardcoded, optional, required)
import RemoteData as RemoteData exposing (..)



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
    , orders : List InvoiceLineItem
    }


init : String -> ( Model, Cmd Msg )
init token =
    ( { token = token
      , searchTerm = "Leroy"
      , invoices = NotAsked
      , invoiceDetails = Nothing
      , orders = []
      }
    , Cmd.none
    )


type Msg
    = SearchInputChanged String
    | LoadInvoices
    | InvoicesLoaded (WebData InvoiceListResponse)
    | InvoiceDetailsReceived (Result Decode.Error (List InvoiceDetail))
    | AddOrder InvoiceLineItem
    | RemoveOrder String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SearchInputChanged new ->
            ( { model | searchTerm = new }, Cmd.none )

        LoadInvoices ->
            ( model, getInvoicesForCompany model )

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
            ( { model | invoices = invoices }, fetchCmd )

        InvoiceDetailsReceived results ->
            ( { model | invoiceDetails = Result.toMaybe results }, Cmd.none )

        AddOrder lineItem ->
            let
                orders =
                    if List.any (\o -> o.id == lineItem.id) model.orders then
                        model.orders

                    else
                        lineItem :: model.orders
            in
            ( { model | orders = orders }, Cmd.none )

        RemoveOrder id ->
            ( { model | orders = List.filter (\o -> o.id /= id) model.orders }, Cmd.none )


view : Model -> Html Msg
view model =
    div
        [ class "content"
        ]
        [ div [ class "header" ]
            [ div [] [ h3 [] [ text "Zoho Helper" ] ]
            , div [ class "search-controls" ]
                [ input [ onInput SearchInputChanged, placeholder "Company Name", value model.searchTerm ] []
                , button [ onClick LoadInvoices ] [ text "Search" ]
                ]
            ]
        , case model.invoiceDetails of
            Just invoiceDetails ->
                let
                    total =
                        List.foldl (\o acc -> o.itemtotal + acc) 0 model.orders
                in
                div [ class "main" ]
                    [ div [ class "invoice-wrapper" ]
                        (List.map invoiceItem invoiceDetails)
                    , div [ class "order-builder" ]
                        ([ h3 [] [ text "Order Builder" ]
                         ]
                            |> pushIf (List.length model.orders > 0)
                                (lineItemTable total model.orders Order)
                        )
                    ]

            _ ->
                div [] []
        ]


pushIf : Bool -> a -> List a -> List a
pushIf pred thing list =
    if pred then
        list ++ [ thing ]

    else
        list


orderItems : List InvoiceLineItem -> Html Msg
orderItems items =
    table []
        [ thead []
            [ tr []
                [ th [] [ text "Product" ]
                ]
            ]
        ]


invoiceItem : InvoiceDetail -> Html Msg
invoiceItem { lineitems, total } =
    div []
        [ lineItemTable total lineitems Invoice
        ]


type ItemTable
    = Invoice
    | Order


lineItemTable : Float -> List InvoiceLineItem -> ItemTable -> Html Msg
lineItemTable total lineItems tableType =
    let
        lineItemRow lineItem =
            let
                { id, name, quantity, itemtotal } =
                    lineItem
            in
            tr [ class "line-item" ]
                [ td [] [ text name ]
                , td [] [ text <| String.fromFloat quantity ]
                , td [] [ text <| String.fromFloat itemtotal ]
                , case tableType of
                    Order ->
                        td [ onClick (RemoveOrder id) ]
                            [ i [ class "fas fa-times-circle" ] [] ]

                    Invoice ->
                        td [ onClick (AddOrder lineItem) ]
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
                , th [] [ text "Controls" ]
                ]
            ]
        , tbody []
            (List.map lineItemRow lineItems)
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



-- Api


apiUrl : String -> String
apiUrl endpoint =
    "https://inventory.zoho.eu/api/v1/" ++ endpoint ++ "?organization_id=20062851724"



-- Invoices


type alias InvoiceListItem =
    { id : String
    , customerName : String
    , date : String
    }


invoiceListItemDecoder : Decoder InvoiceListItem
invoiceListItemDecoder =
    Decode.succeed InvoiceListItem
        |> D.required "invoice_id" Decode.string
        |> D.required "customer_name" Decode.string
        |> D.required "date" Decode.string


type alias InvoiceListResponse =
    { invoices : List InvoiceListItem }


invoiceListResponseDecoder : Decoder InvoiceListResponse
invoiceListResponseDecoder =
    Decode.succeed InvoiceListResponse
        |> D.required "invoices" (Decode.list invoiceListItemDecoder)


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


type alias InvoiceDetailResponse =
    { invoice : InvoiceDetail }


invoiceDetailsResponseDecoder : Decode.Value -> Result Decode.Error (List InvoiceDetail)
invoiceDetailsResponseDecoder =
    Decode.decodeValue
        (Decode.field "invoices" (Decode.list invoiceDetailDecoder))


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
