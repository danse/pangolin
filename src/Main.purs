module Main where


import Prelude
import Data.List( replicate, take, drop )
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Data.Nullable( toMaybe )

import qualified Thermite as T
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Document as DOM
import qualified DOM.Node.Types as DOM
import qualified DOM.HTML.Types as DOM

-- adapt the description length to the amount of minutes passed
crumbify :: String -> Int -> Tuple String String
crumbify description minutes =
  let dots = replicate minutes '.'
      concatenated = description ++ dots
  in Tuple (take minutes concatenated) (drop minutes description)

data Action = Submit Int | Discard

type State = { counter :: Int }

initialState :: State
initialState = { counter: 0 }

render :: T.Render State _ Action
render dispatch _ state _ = [
  R.input [] [],
  R.button [RP.onClick \ _ -> dispatch (Submit state.counter)] [ R.text "Submit" ],
  R.button [RP.onClick \ _ -> dispatch Discard] [ R.text "Discard" ]
]

performAction :: T.PerformAction _ State _ Action
performAction (Submit time) _ state update = update $ state { counter = newCounter }
  where newCounter = state.counter + time
performAction Discard _ state update = update $ state { counter = 0 }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

interface :: R.ReactElement
interface = R.createFactory component {}
  where component = T.createClass spec initialState

getBody :: Eff (dom :: DOM.DOM) (Maybe DOM.Element)
getBody = do
  document <- DOM.window >>= DOM.document
  htmlElement <- DOM.body document
  return (DOM.htmlElementToElement <$> (toMaybe htmlElement))

main :: Eff (dom :: DOM.DOM) Unit
main = do
  Just body <- getBody
  R.render interface body
  return unit

