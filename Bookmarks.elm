module Bookmarks (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp
import Signal exposing (..)


-- MODEL


type alias Category =
  { id : Int
  , name : String
  }


type alias Bookmark =
  { id : Int
  , title : String
  , url : String
  , categoryName : String
  }


type alias Model =
  { bookmarks : List Bookmark
  , categories : List Category
  , currentCategory : Maybe String
  , showCreateBookmark : Bool
  , showUpdateBookmark : Bool
  , maxId : Int
  , newBookmarkURL : String
  , newBookmarkTitle : String
  , updateBookmarkURL : String
  , updateBookmarkTitle : String
  , bookmarkIdBeingEdited : Maybe Int
  }


initCategories : List Category
initCategories =
  [ { id = 0, name = "Development" }
  , { id = 1, name = "Design" }
  , { id = 2, name = "Exercise" }
  , { id = 3, name = "Humor" }
  ]


initBookmarks : List Bookmark
initBookmarks =
  [ { id = 0, title = "AngularJS", url = "http://angularjs.org", categoryName = "Development" }
  , { id = 1, title = "Egghead.io", url = "http://angularjs.org", categoryName = "Development" }
  , { id = 2, title = "A List Apart", url = "http://alistapart.com/", categoryName = "Design" }
  , { id = 3, title = "One Page Love", url = "http://onepagelove.com/", categoryName = "Design" }
  , { id = 4, title = "MobilityWOD", url = "http://www.mobilitywod.com/", categoryName = "Exercise" }
  , { id = 5, title = "Robb Wolf", url = "http://robbwolf.com/", categoryName = "Exercise" }
  , { id = 6, title = "Senor Gif", url = "http://memebase.cheezburger.com/senorgif", categoryName = "Humor" }
  , { id = 7, title = "Wimp", url = "http://wimp.com", categoryName = "Humor" }
  , { id = 8, title = "Dump", url = "http://dump.com", categoryName = "Humor" }
  ]


initModel =
  { bookmarks = initBookmarks
  , categories = initCategories
  , currentCategory = Nothing
  , showCreateBookmark = False
  , showUpdateBookmark = False
  , maxId = 8
  , newBookmarkURL = ""
  , newBookmarkTitle = ""
  , updateBookmarkURL = ""
  , updateBookmarkTitle = ""
  , bookmarkIdBeingEdited = Nothing
  }



-- UPDATE


type Action
  = SelectCategory String
  | AddBookmark
  | UpdateURL String
  | UpdateTitle String
  | UpdateBookmark Int
  | DeleteBookmark Int
  | ToggleCreateBookmarkForm
  | ToggleUpdateBookmarkForm Int
  | EditUrl String
  | EditTitle String


update : Action -> Model -> Model
update action model =
  case action of
    SelectCategory categoryName ->
      { model | currentCategory = Just categoryName }

    UpdateURL url ->
      { model | newBookmarkURL = url }

    UpdateTitle title ->
      { model | newBookmarkTitle = title }

    EditUrl url ->
      { model | updateBookmarkURL = url }

    EditTitle title ->
      { model | updateBookmarkTitle = title }

    AddBookmark ->
      let
        newMaxId =
          model.maxId + 1

        newBookmark =
          { id = newMaxId
          , title = model.newBookmarkTitle
          , url = model.newBookmarkURL
          , categoryName = Maybe.withDefault "" model.currentCategory
          }

        newBookmarks =
          model.bookmarks ++ [ newBookmark ]
      in
        { model
          | bookmarks = newBookmarks
          , maxId = newMaxId
          , newBookmarkURL = ""
          , newBookmarkTitle = ""
          , showCreateBookmark = False
        }

    UpdateBookmark id ->
      let
        updateBookmark bookmark =
          if bookmark.id == id then
            { bookmark | url = model.updateBookmarkURL, title = model.updateBookmarkTitle }
          else
            bookmark

        updatedBookmarks =
          model.bookmarks
            |> List.map updateBookmark
      in
        { model
        | bookmarks = updatedBookmarks
        , updateBookmarkURL = ""
        , updateBookmarkTitle = ""
        , showUpdateBookmark = False
        , bookmarkIdBeingEdited = Nothing
        }

    {- -
    AddBookmark partialBookmark ->
      let
        newId = model.maxId + 1
        bookMark =
          { id = newId
          , title = partialBookmark.title
          , url = partialBookmark.url
          , categoryName = model.currentCategory
          }
        newBookmarks = bookMark :: model.bookmarks
      in
        { model |
          bookmarks = newBookmarks
        , maxId = newId
        }
    -
    -}

    ToggleCreateBookmarkForm ->
      { model | showCreateBookmark = not model.showCreateBookmark }

    ToggleUpdateBookmarkForm id ->
      let
        bookmarkBeingEdited : Maybe Bookmark
        bookmarkBeingEdited =
          model.bookmarks
            |> List.filter (\bookmark -> bookmark.id == id)
            |> List.head

      in
        case bookmarkBeingEdited of
          Just bookmark ->
            { model
            | showUpdateBookmark = not model.showUpdateBookmark
            , updateBookmarkURL = bookmark.url
            , updateBookmarkTitle = bookmark.title
            , bookmarkIdBeingEdited = Just id
            }
          Nothing ->
            model

    DeleteBookmark id ->
      let
        newBookmarksList =
          model.bookmarks |> List.filter (\bookmark -> bookmark.id /= id)
      in
        { model | bookmarks = newBookmarksList }




-- VIEW


sidebar : Address Action -> List Category -> Html
sidebar address categories =
  div
    [ class "col-sm-3 col-md-2 sidebar" ]
    [ a
        []
        [ img [ class "logo", src "assets/img/eggly-logo.png" ] [] ]
    , ul
        [ class "nav nav-sidebar" ]
        (List.map (\cat -> categoryRow address cat.name) categories)
    ]


categoryRow : Address Action -> String -> Html
categoryRow address categoryName =
  li
    []
    [ a
        [ onClick address (SelectCategory categoryName) ]
        [ text categoryName ]
    ]


bookmarkList : Address Action -> List Bookmark -> Maybe String -> Html
bookmarkList address bookmarks currentCategory =
  let
    currentCat =
      Maybe.withDefault "" currentCategory

    predicate bookmark =
      case currentCategory of
        Nothing ->
          True

        Just categoryName ->
          bookmark.categoryName == categoryName

    filteredBookmarks =
      bookmarks |> List.filter predicate
  in
    div
      []
      (List.map (bookmarkRow address) filteredBookmarks)


bookmarkRow : Address Action -> Bookmark -> Html
bookmarkRow address bookmark =
  div
    []
    [ button
        [ class "close", onClick address (DeleteBookmark bookmark.id) ]
        [ text "x" ]
    , button
        [ class "btn btn-link" ]
        [ span
            [ class "glyphicon glyphicon-pencil", onClick address (ToggleUpdateBookmarkForm bookmark.id) ]
            []
        ]
    , a
        [ href bookmark.url, target "_blank" ]
        [ text bookmark.title ]
    ]


hiddenStyle =
  style
    [ ( "display", "none" ) ]


showStyle =
  style
    [ ( "display", "block" ) ]


createBookmarkForm : Address Action -> Model -> Html
createBookmarkForm address model =
  let
    visibilityStyle =
      if model.currentCategory /= Nothing then
        showStyle
      else
        hiddenStyle
  in
    div
      []
      [ button
          [ visibilityStyle
          , class "btn btn-link"
          , type' "button"
          , onClick address ToggleCreateBookmarkForm
          ]
          [ span
              [ class "glyphicon glyphicon-plus" ]
              []
          , text "Create Bookmark"
          ]
      , div
          [ classList
              [ ( "in", model.showCreateBookmark )
              , ( "create-form collapse", True )
              ]
          ]
          [ div
              [ class "form-group" ]
              [ label
                  [ for "newBookmarkTitle" ]
                  [ text "Bookmark Title" ]
              , input
                  [ class "form-control"
                  , id "newBookmarkTitle"
                  , placeholder "Enter title"
                  , type' "text"
                  , value model.newBookmarkTitle
                  , on "input" targetValue (Signal.message address << UpdateTitle)
                  ]
                  []
              ]
          , div
              [ class "form-group" ]
              [ label
                  [ for "newBookmarkURL" ]
                  [ text "Bookmark URL" ]
              , input
                  [ class "form-control"
                  , id "newBookmarkURL"
                  , placeholder "Enter URL"
                  , type' "text"
                  , value model.newBookmarkURL
                  , on "input" targetValue (Signal.message address << UpdateURL)
                  ]
                  []
              ]
          , button
              [ class "btn btn-info btn-lg"
              , type' "submit"
              , onClick address AddBookmark
              ]
              [ text "Create" ]
          , button
              [ class "btn btn-default btn-lg pull-right", type' "button" ]
              [ text "Cancel" ]
          ]
      ]


updateBookmarkForm : Address Action -> Model -> Html
updateBookmarkForm address model =
  let
    visibilityStyle =
      if model.showUpdateBookmark then
        showStyle
      else
        hiddenStyle

    bkId = Maybe.withDefault -1 model.bookmarkIdBeingEdited
  in
    div
      [ visibilityStyle ]
      [ h1
          [ ]
          [ text "Update Bookmark" ]
      , div
          [ class "create-form" ]
          [ div
              [ class "form-group" ]
              [ label
                  [ for "updateBookmarkTitle" ]
                  [ text "Bookmark Title" ]
              , input
                  [ class "form-control"
                  , id "updateBookmarkTitle"
                  , placeholder "Enter title"
                  , type' "text"
                  , value model.updateBookmarkTitle
                  , on "input" targetValue (Signal.message address << EditTitle)
                  ]
                  []
              ]
          , div
              [ class "form-group" ]
              [ label
                  [ for "updateBookmarkURL" ]
                  [ text "Bookmark URL" ]
              , input
                  [ class "form-control"
                  , id "updateBookmarkURL"
                  , placeholder "Enter URL"
                  , type' "text"
                  , value model.updateBookmarkURL
                  , on "input" targetValue (Signal.message address << EditUrl)
                  ]
                  []
              ]
          , button
              [ class "btn btn-info btn-lg"
              , type' "submit"
              , onClick address (UpdateBookmark bkId)
              ]
              [ text "Update" ]
          , button
              [ class "btn btn-default btn-lg pull-right", type' "button" ]
              [ text "Cancel" ]
          ]
      ]

view : Address Action -> Model -> Html
view address model =
  div
    [ class "row" ]
    [ sidebar address model.categories
    , div
        [ class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main" ]
        [ bookmarkList address model.bookmarks model.currentCategory
        , createBookmarkForm address model
        , updateBookmarkForm address model
        ]
    ]


main : Signal Html
main =
  StartApp.start
    { model = initModel
    , update = update
    , view = view
    }
