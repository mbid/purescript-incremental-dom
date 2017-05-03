module Web.IncrementalDOM
( IDOM
, elementOpen
, elementOpenStart
, attr
, elementOpenEnd
, elementClose
, elementVoid
, text
, patch
, currentElement
, currentPointer
, skip
, skipNode
)
where

import Prelude
import Control.Monad.Eff (Eff, kind Effect)
import DOM (DOM)
import DOM.HTML.Types (HTMLElement)
import DOM.Node.Types (Node, Text)
import Data.Bifunctor (bimap)
import Data.Foreign (Foreign, isNull, isUndefined, toForeign, unsafeFromForeign)
import Data.Function.Uncurried (Fn3, Fn4, runFn3, runFn4)
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

foreign import data IDOM :: Effect

foreign import foreignNull :: Foreign

maybeToForeign :: forall a. Maybe a -> Foreign
maybeToForeign Nothing = foreignNull
maybeToForeign (Just x) = toForeign x

tupleToList :: forall a. Tuple a a -> Array a
tupleToList (Tuple x1 x2) = [ x1, x2 ]

tupleArrayToForeign :: forall a b. Array (Tuple a b) -> Foreign
tupleArrayToForeign xs = toForeign flatXs
  where
    flatXs :: Array Foreign
    flatXs = xs >>= bimap toForeign toForeign >>> tupleToList


foreign import elementOpenImpl ::
  forall e.
  Fn4 Foreign Foreign Foreign Foreign (Eff (idom :: IDOM | e) HTMLElement)

-- | `elementOpen tag key staticProperties properties` declares a `tag`
-- | Element with optional `key`, a list of (unchecked) `staticProperties` and
-- | (checked) `dynamicProperties` at the current location in the document.
elementOpen ::
  forall e.
  String ->  -- ^ asdfasdf
  Maybe String ->
  Array (Tuple String Foreign) -> Array (Tuple String Foreign) ->
  Eff (idom :: IDOM | e) HTMLElement
elementOpen tagname key staticProperties properties =
  runFn4
  elementOpenImpl
  (toForeign tagname)
  (maybeToForeign key)
  (tupleArrayToForeign staticProperties)
  (tupleArrayToForeign properties)

foreign import elementOpenStartImpl ::
  forall e.  Fn3 Foreign Foreign Foreign (Eff (idom :: IDOM | e) Unit)
-- | Used with `attr` and `elementOpenEnd` to declare an element. 
-- | `elementOpenStart tag key staticProperties` declares the start of the
-- | attribute list of a `tag` Element with optional `key` and a list of
-- | (unchecked) `staticProperties` at the current location in the document
-- | tree.
elementOpenStart ::
  forall e.
  String -> Maybe String -> Array (Tuple String Foreign) ->
  Eff (idom :: IDOM | e) Unit
elementOpenStart tagname key staticProperties =
  runFn3
  elementOpenStartImpl
  (toForeign tagname)
  (maybeToForeign key)
  (tupleArrayToForeign staticProperties)

-- | Used with `attr` and `elementOpenEnd` to declare an element. 
foreign import attr :: forall e. String -> Foreign -> Eff (idom :: IDOM | e) Unit

-- | Used with `attr` and `elementOpenEnd` to declare an element. 
foreign import elementOpenEnd :: forall e. Eff (idom :: IDOM | e) HTMLElement

-- | Signifies the end of the element opened with `elementOpen`, corresponding
-- | to a closing tag (e.g. `</div>` in HTML). Any childNodes of the currently
-- | open Element that are in the DOM that have not been encountered in the
-- | current render pass are removed by the call to `elementClose`.
foreign import elementClose ::
  forall e. String -> (Eff (idom :: IDOM | e) HTMLElement)

foreign import elementVoidImpl ::
  forall e.
  Fn4 Foreign Foreign Foreign Foreign (Eff (idom :: IDOM | e) HTMLElement)
-- | `elementVoid tag key staticProperties properties` declares a `tag`
-- | Element with optional `key`, a list of (unchecked) `staticProperties` and
-- | (checked) `dynamicProperties` at the current location in the document and
-- | closes it immediately.
elementVoid ::
  forall e.
  String -> Maybe String ->
  Array (Tuple String Foreign) -> Array (Tuple String Foreign) ->
  Eff (idom :: IDOM | e) HTMLElement
elementVoid tagname key staticProperties properties =
  runFn4
  elementVoidImpl
  (toForeign tagname)
  (maybeToForeign key)
  (tupleArrayToForeign staticProperties)
  (tupleArrayToForeign properties)

-- | Declares a Text node, with the specified text, should appear at the
-- | current location in the document tree.  
-- | The bindings do not support formatters at the moment.
foreign import text :: forall e. String -> Eff (idom :: IDOM | e) Text

-- | Updates the provided Node with a function containing zero or more calls to
-- | `elementOpen`, `text` and `elementClose`. The provided callback function
-- | may call other such functions. The patch function may be called with a new
-- | Node while a call to patch is already executing. 
foreign import patch ::
  forall e.
  Node ->
  Eff (idom :: IDOM, dom :: DOM | e) Unit ->
  Eff (dom :: DOM | e) Unit

-- undocumented functions

foreign import currentElement :: forall e. Eff (idom :: IDOM | e) Node

foreign import currentPointerImpl :: forall e. Eff (idom :: IDOM | e) Foreign
currentPointer :: forall e. Eff (idom :: IDOM | e) (Maybe Node)
currentPointer = do
  f <- currentPointerImpl
  pure $ if isUndefined f || isNull f then Nothing else Just (unsafeFromForeign f)

foreign import skip :: forall e. Eff (idom :: IDOM | e) Unit

foreign import skipNode :: forall e. Eff (idom :: IDOM | e) Unit
