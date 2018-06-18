module Multiselect.SelectCss
    exposing
        ( menuItem
        , menuItemHovered
        , menu
        , clear
        , clearWrap
        , arrow
        , arrowWrap
        , arrowUpside
        , tagLabel
        , tagIcon
        , tag
        , tagWrap
        , input
        , inputMirrow
        , wrap
        , inputWrap
        , container
        , opened
        , focused
        , itemHeight
        , menuHeight
        )

import Css
    exposing
        ( Style
        , property
        , px
        , hex
        , int
        , pct
        , width
        , height
        , hover
        , num
        , cursor
        , padding3
        , rgba
        , solid
        , em
        , border2
        , color
        , none
        )
import Css.Foreign exposing (children, everything, class)


boxShadowCustom : String -> Style
boxShadowCustom p =
    property "box-shadow" <| p


itemHeight : number
itemHeight =
    32


menuHeight : number
menuHeight =
    200


wrap : Style
wrap =
    Css.batch
        [ Css.position Css.relative
        , Css.width (pct 100)
        ]


container : Style
container =
    Css.batch
        [ Css.border3 (px 1) solid (hex "#ccc")
        , Css.borderRadius (px 4)
        , Css.borderColor3 (hex "#d9d9d9") (hex "#ccc") (hex "#b3b3b3")
        , Css.backgroundColor (hex "#fff")
        , Css.color (hex "#333")
        , Css.height (px 34)
        , Css.width (Css.pct 100)
        , Css.display Css.table
        ]


inputWrap : Style
inputWrap =
    Css.batch
        [ Css.display Css.inlineBlock
        , Css.marginLeft (px 5)
        , Css.padding Css.zero
        , Css.verticalAlign Css.middle
        , Css.paddingBottom (px 8)
        ]


input : Style
input =
    Css.batch
        [ Css.borderStyle none
        , Css.border Css.zero
        , Css.lineHeight (px 14)
        , Css.outlineStyle none
        , Css.fontSize Css.inherit
        , Css.lineHeight (int 1)
        , Css.padding Css.zero
        , Css.paddingTop (px 8)
        ]


inputMirrow : Style
inputMirrow =
    Css.batch
        [ Css.position Css.absolute
        , Css.top (px -100)
        , Css.left (px -100)
        , Css.height Css.zero
        , Css.overflow Css.scroll
        , Css.fontWeight Css.normal
        , Css.fontStyle Css.normal
        , Css.fontSize Css.inherit
        , Css.lineHeight (int 1)
        ]


focused : Style
focused =
    Css.batch
        [ Css.borderColor (hex "#007eff")
        , boxShadowCustom "inset 0 1px 1px rgba(0, 0, 0, 0.075), 0 0 0 3px rgba(0, 126, 255, 0.1)"
        ]


opened : Style
opened =
    Css.batch
        [ Css.borderBottomLeftRadius Css.zero
        , Css.borderBottomRightRadius Css.zero
        ]


tagWrap : Style
tagWrap =
    Css.batch
        [ Css.display Css.inline
        ]


tag : Style
tag =
    Css.batch
        [ color (hex "#007eff")
        , border2 (px 1) solid
        , Css.borderColor (Css.rgba 0 126 255 0.24)
        , Css.borderRadius (px 2)
        , Css.backgroundColor (Css.rgba 0 126 255 0.08)
        , Css.display Css.inlineBlock
        , Css.fontSize (em 0.9)
        , Css.lineHeight (num 1.4)
        , Css.marginLeft (px 5)
        , Css.marginTop (px 5)
        , Css.verticalAlign Css.top
        ]


tagIcon : Style
tagIcon =
    Css.batch
        [ Css.hover [ Css.backgroundColor (hex "#d8eafd") ]
        , Css.cursor Css.pointer
        , Css.borderRight3 (px 1) solid (rgba 0 126 255 0.24)
        , padding3 (px 1) (px 5) (px 3)
        , Css.display Css.inlineBlock
        , Css.verticalAlign Css.middle
        ]


tagLabel : Style
tagLabel =
    Css.batch
        [ Css.padding2 (px 2) (px 5)
        , Css.display Css.inlineBlock
        , Css.verticalAlign Css.middle
        ]


clearWrap : Style
clearWrap =
    Css.batch
        [ Css.width (px 17)
        , Css.color (hex "#999")
        , cursor Css.pointer
        , Css.display Css.tableCell
        , Css.position Css.relative
        , Css.textAlign Css.center
        , Css.verticalAlign Css.middle
        , hover
            [ Css.color (hex "#D0021B") ]
        ]


clear : Style
clear =
    Css.batch
        [ Css.display Css.inlineBlock
        , Css.fontSize (px 18)
        , Css.lineHeight (num 1)
        ]


arrowWrap : Style
arrowWrap =
    Css.batch
        [ Css.cursor Css.pointer
        , Css.display Css.tableCell
        , Css.position Css.relative
        , Css.textAlign Css.center
        , Css.verticalAlign Css.middle
        , Css.width (px 25)
        , Css.paddingRight (px 5)
        , hover
            [ children
                [ Css.Foreign.selector "*[rel=arrow]"
                    [ Css.borderTopColor (hex "#666") ]
                , Css.Foreign.selector "*[rel=arrowUpside]"
                    [ Css.borderBottomColor (hex "#666")
                    ]
                ]
            ]
        ]


arrow : Style
arrow =
    Css.batch
        [ Css.borderColor3 (hex "#999") Css.transparent Css.transparent
        , Css.borderStyle Css.solid
        , Css.borderTopWidth (px 5)
        , Css.borderLeftWidth (px 5)
        , Css.borderRightWidth (px 5)
        , Css.borderBottomWidth (px 2.5)
        , Css.display Css.inlineBlock
        , height Css.zero
        , width Css.zero
        , Css.position Css.relative
        ]


arrowUpside : Style
arrowUpside =
    Css.batch
        [ Css.borderColor3 Css.transparent Css.transparent (hex "#999")
        , Css.borderStyle Css.solid
        , Css.borderTopWidth (px 2.5)
        , Css.borderLeftWidth (px 5)
        , Css.borderRightWidth (px 5)
        , Css.borderBottomWidth (px 5)
        , Css.display Css.inlineBlock
        , height Css.zero
        , width Css.zero
        , Css.position Css.relative
        , Css.top (px -2.5)
        ]


menu : Style
menu =
    Css.batch
        [ Css.borderBottomRightRadius (px 4)
        , Css.borderBottomLeftRadius (px 4)
        , Css.backgroundColor (hex "#fff")
        , Css.border3 (px 1) Css.solid (hex "#ccc")
        , Css.borderTopColor (hex "#e6e6e6")
        , Css.boxShadow4 Css.zero (px 1) Css.zero (Css.rgba 0 0 0 0.06)
        , Css.marginTop (px -1)
        , Css.maxHeight (px menuHeight)
        , Css.position Css.absolute
        , Css.width (pct 100)
        , Css.zIndex (int 1)
        , Css.overflowY Css.scroll
        ]


menuItem : Style
menuItem =
    Css.batch
        [ Css.color (hex "#666")
        , Css.cursor Css.pointer
        , Css.padding2 (px 8) (px 10)
        , Css.maxHeight (px itemHeight)
        ]


menuItemHovered : Style
menuItemHovered =
    Css.batch
        [ Css.backgroundColor (Css.rgba 0 126 255 0.08)
        , Css.color (hex "#333")
        ]
