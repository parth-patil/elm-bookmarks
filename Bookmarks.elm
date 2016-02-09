module Bookmarks (..) where

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import StartApp.Simple as StartApp
import Signal exposing (..)
import String


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
  , updateBookmarkURL : String
  , updateBookmarkTitle : String
  , bookmarkIdBeingEdited : Maybe Int
  , searchTerm : String
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
  , updateBookmarkURL = ""
  , updateBookmarkTitle = ""
  , bookmarkIdBeingEdited = Nothing
  , searchTerm = ""
  }



-- UPDATE


type Action
  = SelectCategory String
  | AddBookmark
  | UpdateBookmark Int
  | DeleteBookmark Int
  | ToggleCreateBookmarkForm
  | ToggleUpdateBookmarkForm Int
  | EditUrl String
  | EditTitle String
  | HideCreateForm
  | HideUpdateForm
  | UpdateSearchTerm String


update : Action -> Model -> Model
update action model =
  case action of
    SelectCategory categoryName ->
      { model | currentCategory = Just categoryName }

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
          , title = model.updateBookmarkTitle
          , url = model.updateBookmarkURL
          , categoryName = Maybe.withDefault "" model.currentCategory
          }

        newBookmarks =
          model.bookmarks ++ [ newBookmark ]
      in
        { model
          | bookmarks = newBookmarks
          , maxId = newMaxId
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

    ToggleCreateBookmarkForm ->
      { model
        | showCreateBookmark = True
        , showUpdateBookmark = False
        , updateBookmarkURL = ""
        , updateBookmarkTitle = ""
      }

    HideCreateForm ->
      { model | showCreateBookmark = False }

    HideUpdateForm ->
      { model | showUpdateBookmark = False }

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
            | showUpdateBookmark = True
            , updateBookmarkURL = bookmark.url
            , updateBookmarkTitle = bookmark.title
            , bookmarkIdBeingEdited = Just id
            , showCreateBookmark = False
            }
          Nothing ->
            model

    DeleteBookmark id ->
      let
        newBookmarksList =
          model.bookmarks |> List.filter (\bookmark -> bookmark.id /= id)
      in
        { model | bookmarks = newBookmarksList }

    UpdateSearchTerm term ->
      { model | searchTerm = term }




-- VIEW


sidebar : Address Action -> List Category -> Maybe String -> Html
sidebar address categories currentCategory =
  div
    [ class "col-sm-3 col-md-2 sidebar" ]
    [ a
        []
        [ img [ class "logo", src "assets/img/eggly-logo.png" ] [] ]
    , ul
        [ class "nav nav-sidebar" ]
        (List.map (\cat -> categoryRow address cat.name currentCategory) categories)
    ]


categoryRow : Address Action -> String ->  Maybe String -> Html
categoryRow address categoryName currentCategory =
  let
    currCat = Maybe.withDefault "" currentCategory
    categoryActive =
      categoryName == currCat
  in
    li
      [ classList [("active",  categoryActive) ]]
      [ a
          [ onClick address (SelectCategory categoryName) ]
          [ text categoryName ]
    ]


bookmarkList : Address Action -> Model -> Html
bookmarkList address model =
  let
    currentCat =
      Maybe.withDefault "" model.currentCategory

    searchFilter bookmark =
      bookmark.title
        |> String.toLower
        |> String.contains (String.toLower model.searchTerm)

    categoryFilter bookmark =
      if currentCat == "" then
        True
      else
        bookmark.categoryName == currentCat

    filteredBookmarks =
      model.bookmarks
        |> List.filter categoryFilter
        |> List.filter searchFilter

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

bookmarkForm: Address Action -> Model  -> Html
bookmarkForm address model =
   div
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
        ]

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
          [ bookmarkForm address model
          , button
              [ class "btn btn-info btn-lg"
              , type' "submit"
              , onClick address AddBookmark
              ]
              [ text "Create" ]
          , button
              [ class "btn btn-default btn-lg pull-right", type' "button" ,  onClick address HideCreateForm ]
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
      , bookmarkForm address model
          , button
              [ class "btn btn-info btn-lg"
              , type' "submit"
              , onClick address (UpdateBookmark bkId)
              ]
              [ text "Update" ]
          , button
              [ class "btn btn-default btn-lg pull-right", type' "button", onClick  address HideUpdateForm ]
              [ text "Cancel" ]
          ]


searchBar : Address Action -> String -> Html
searchBar address searchTerm =
  div
    []
    [ input
      [ class "form-control"
      , id "searchTerm"
      , placeholder "Filter Bookmarks"
      , type' "text"
      , value searchTerm
      , on "input" targetValue (Signal.message address << UpdateSearchTerm)
      ]
      []
    ]

view : Address Action -> Model -> Html
view address model =
  div
    [ class "row" ]
    [ sidebar address model.categories model.currentCategory
    , div
        [ class "col-sm-9 col-sm-offset-3 col-md-10 col-md-offset-2 main" ]
        [ searchBar address model.searchTerm
        , bookmarkList address model
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
