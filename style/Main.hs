{-# language OverloadedStrings #-}
import Stitch
import Stitch.Combinators

style :: CSS
style = do
  ".wrapper" ? do
    "text-align" .= "center"
  ".container" ? do
    "display" .= "inline-block"
    "text-align" .= "left"
    "margin" .= "10% 0"

main = printCSS style
