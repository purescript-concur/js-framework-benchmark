module Main where

import Concur.Core (Widget)
import Concur.React (HTML)
import Concur.React.DOM as D
import Concur.React.Props as P
import Concur.React.Run (runWidgetInDom)
import Control.Applicative (pure)
import Control.Bind (bind)
import Control.Monad (class Monad)
import Data.Array as Array
import Data.CommutativeRing ((+))
import Data.Eq ((/=), (==))
import Data.EuclideanRing (mod)
import Data.Function (($))
import Data.Functor (map, ($>))
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Show (show)
import Data.Unit (Unit)
import Effect (Effect)
import Effect.Class (class MonadEffect, liftEffect)
import Effect.Uncurried (EffectFn5, runEffectFn5)

main :: Effect Unit
main = runWidgetInDom "root" $ tea initialState render handleAction

tea :: forall a s m x. Monad m => MonadEffect m => s -> (s -> m a) -> (a -> s -> Effect s) -> m x
tea s r u = go s
  where
  go st = do
    a <- r st
    st' <- liftEffect (u a st)
    go st'

render :: State -> Widget HTML Action
render state =
  D.div
    [ P.className "container" ]
    [ jumbotron
    , D.table
        [ P.className "table table-hover table-striped test-data" ]
        [ D.tbody' do
            map (renderRow state.selectedId) state.rows
        ]
    , footer
    ]

handleAction :: Action -> State -> Effect State
handleAction action state = case action of
  Create amount -> do
    newRows <- createRandomNRows amount state.lastId
    pure state { rows = newRows, lastId = state.lastId + amount }

  AppendOneThousand -> do
    let amount = 1000
    newRows <- createRandomNRows amount state.lastId
    pure state { rows = state.rows <> newRows, lastId = state.lastId + amount }

  UpdateEveryTenth -> do
    let
      updateLabel ix row =
        if ix `mod` 10 == 0 then row { label = row.label <> " !!!" } else row

    pure state { rows = Array.mapWithIndex updateLabel state.rows }

  Clear ->
    pure state { rows = [] }

  Swap -> do
    case swapRows state.rows 1 998 of
      Nothing -> pure state
      Just rows -> pure state { rows = rows }

  Remove id ->
      pure state { rows = Array.filter (\r -> r.id /= id) state.rows }

  Select id -> do
    if state.selectedId == id then
      pure state
    else
      pure state { selectedId = id }

data Action
  = Create Int
  | AppendOneThousand
  | UpdateEveryTenth
  | Clear
  | Swap
  | Remove Int
  | Select Int

type State =
  { rows :: Array RowDef
  , lastId :: Int
  , selectedId :: Int
  }

type RowDef =
  { id :: Int
  , label :: String
  }

initialState :: State
initialState = { rows: [], lastId: 0, selectedId: 0 }

type ActionButton = { id :: String, label :: String, action :: Action }

buttons :: Array ActionButton
buttons =
    [ { id: "run", label: "Create 1,000 rows", action: Create 1000 }
    , { id: "runlots", label: "Create 10,000 rows", action: Create 10000 }
    , { id: "add", label: "Append 1,000 rows", action: AppendOneThousand }
    , { id: "update", label: "Update every 10th row", action: UpdateEveryTenth }
    , { id: "clear", label: "Clear", action: Clear }
    , { id: "swaprows", label: "Swap Rows", action: Swap }
    ]

renderActionButton :: ActionButton -> Widget HTML Action
renderActionButton { id, label, action } =
  D.div
    [ P.className "col-sm-6 smallpad" ]
    [ D.button
        [ P._type "button"
        , P.className "btn btn-primary btn-block"
        , P._id id
        -- , P.unsafeMkProp "ref" "text"
        , P.onClick $> action
        ]
        [ D.text label ]
    ]

renderRow :: Int -> RowDef -> Widget HTML Action
renderRow selectedId row =
  D.tr
    (if selectedId == row.id then
      [ P.className "danger"
      , P.selected true
      ]
    else
      [ ]
    )
    [ D.td colMd1 [ D.text (show row.id) ]
    , D.td colMd4 [ D.a [ P.onClick $> Select row.id ] [ D.text row.label ] ]
    , D.td colMd1 [ D.a [ P.onClick $> Remove row.id ] removeIcon ]
    , spacer
    ]


colMd1 :: forall a. Array (P.ReactProps a)
colMd1 = [ P.className "col-md-1" ]

colMd4 :: forall a. Array (P.ReactProps a)
colMd4 = [ P.className "col-md-4" ]

spacer :: forall a. Widget HTML a
spacer = D.td [ P.className "col-md-6" ] []

removeIcon :: Array (Widget HTML Action)
removeIcon =
  [ D.span
      [ P.className "glyphicon glyphicon-remove"
      , P.unsafeMkProp "aria-hidden" "true"
      ]
      []
  ]
footer :: Widget HTML Action
footer =
  D.span
    [ P.className "preloadicon glyphicon glyphicon-remove"
    , P.unsafeMkProp "aria-hidden" "true"
    ]
    []

jumbotron :: Widget HTML Action
jumbotron =
  D.div
    [ P.className "jumbotron" ]
    [ D.div
        [ P.className "row" ]
        [ D.div
            [ P.className "col-md-6" ]
            [ D.h1' [ D.text "PureScript Concur (non-keyed)" ] ]
            , D.div [ P.className "col-md-6" ] do
                map renderActionButton buttons
            ]
        ]

updateEveryTenth :: Array RowDef -> Array RowDef
updateEveryTenth =
  Array.mapWithIndex updateRowLabel
  where
  updateRowLabel ix row =
    if ix `mod` 10 == 0 then row { label = row.label <> " !!!" } else row

swapRows :: Array RowDef -> Int -> Int -> Maybe (Array RowDef)
swapRows arr index1 index2 = do
  rowA <- arr Array.!! index1
  rowB <- arr Array.!! index2
  arrA <- Array.updateAt index1 rowB arr
  arrB <- Array.updateAt index2 rowA arrA
  pure arrB

foreign import createRandomNRowsImpl :: EffectFn5 (Array String) (Array String) (Array String) Int Int (Array RowDef)

createRandomNRows :: Int -> Int -> Effect (Array RowDef)
createRandomNRows n lastId = runEffectFn5 createRandomNRowsImpl adjectives colours nouns n lastId

adjectives :: Array String
adjectives =
    [ "pretty"
    , "large"
    , "big"
    , "small"
    , "tall"
    , "short"
    , "long"
    , "handsome"
    , "plain"
    , "quaint"
    , "clean"
    , "elegant"
    , "easy"
    , "angry"
    , "crazy"
    , "helpful"
    , "mushy"
    , "odd"
    , "unsightly"
    , "adorable"
    , "important"
    , "inexpensive"
    , "cheap"
    , "expensive"
    , "fancy"
    ]

colours :: Array String
colours =
    [ "red"
    , "yellow"
    , "blue"
    , "green"
    , "pink"
    , "brown"
    , "purple"
    , "brown"
    , "white"
    , "black"
    , "orange"
    ]

nouns :: Array String
nouns =
    [ "table"
    , "chair"
    , "house"
    , "bbq"
    , "desk"
    , "car"
    , "pony"
    , "cookie"
    , "sandwich"
    , "burger"
    , "pizza"
    , "mouse"
    , "keyboard"
    ]
