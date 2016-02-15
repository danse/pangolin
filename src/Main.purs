module Main where

import Prelude
import Data.Maybe
import Data.Tuple
import Control.Monad.Eff
import Data.Nullable( toMaybe )
import Data.Foldable( foldlDefault, foldrDefault )
import Unsafe.Coerce( unsafeCoerce )
import Control.Monad.Eff.Console

import qualified Data.String as S
import qualified Data.List as L
import qualified Data.Array as A

import qualified Thermite as T
import qualified React as R
import qualified React.DOM as R
import qualified React.DOM.Props as RP

import qualified DOM as DOM
import qualified DOM.HTML as DOM
import qualified DOM.HTML.Document as DOM
import qualified DOM.HTML.Window as DOM
import qualified DOM.Node.Types as DOM
import qualified DOM.HTML.Types as DOM

data Action = Submit | Discard | SetCurrent String

type Record = { description :: String }

type State = {
  records :: L.List Record,
  current :: String
  }

initialState :: State
initialState = {
  records: L.Nil,
  current: ""
  }

listToArray :: forall a. L.List a -> Array a
listToArray l = foldrDefault A.cons [] l

render :: T.Render State _ Action
render dispatch _ state _ = [
  R.div [RP.className "wrapper"] [
      R.div [RP.className "container"] [
          R.input [
              RP.value state.current,
              RP.onInput \ e -> dispatch (SetCurrent (unsafeCoerce e).target.value)
              ] [],
          R.button [RP.onClick \ _ -> dispatch Submit] [R.text "Submit" ],
          R.button [RP.onClick \ _ -> dispatch Discard] [R.text "Discard" ],
          R.div [] (listToArray ((\ r -> R.div [][R.text r.description]) <$> state.records))
          ]
      ]
  ]

performAction :: T.PerformAction _ State _ Action
performAction Submit _ state update = update $ state { records = L.Cons { description: state.current } state.records, current = "" }
performAction Discard _ state update = update $ state { current = "" }
performAction (SetCurrent desc) _ state update = update $ state { current = desc }

spec :: T.Spec _ State _ Action
spec = T.simpleSpec performAction render

interface :: R.ReactElement
interface = R.createFactory component {}
  where component = T.createClass spec initialState

getBody :: forall eff. Eff (dom :: DOM.DOM | eff) (Maybe DOM.Element)
getBody = do
  document <- DOM.window >>= DOM.document
  htmlElement <- DOM.body document
  return (DOM.htmlElementToElement <$> (toMaybe htmlElement))

renderOrPass :: forall eff. Maybe DOM.Element -> Eff ( dom :: DOM.DOM | eff) (Maybe R.ReactElement)
renderOrPass (Just body) = Just <$> R.render interface body
renderOrPass Nothing   = pure Nothing

main :: Eff (dom :: DOM.DOM, console :: CONSOLE) (Maybe R.ReactElement)
main = do
  maybeBody <- getBody
  if isNothing maybeBody
    then error "no body found"
    else log "found body"
  renderOrPass maybeBody
