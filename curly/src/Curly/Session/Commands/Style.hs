{-# LANGUAGE CPP, ExistentialQuantification, ViewPatterns, RecursiveDo, QuasiQuotes #-}
module Curly.Session.Commands.Style where

import Curly.Core.Parser
import Curly.Core.Documentation
import Curly.Style
import Data.IORef 
import Language.Format hiding (space)
import Curly.Session.Commands.Common

styleCmd :: Interactive Command

styleDoc = [q_string|
{title Style Documentation}
{p {em Usage:} style TAG STYLE VALUE}
{p Changes the style of the given class of tags to match the value when showing documentation.}
{p {em Example:} style title color ff0000}
|]
styleCmd = withDoc styleDoc $ False <$ do
  tag <- nbsp >> dirArg
  let hexColor = do
        (r,g,b) <- liftA3 (,,) hex2 hex2 hex2
        return $ ColorNumber $ 16 + 36*((r*6) `div` 256) + 6*((g*6) `div` 256) + ((b*6) `div` 256)
      hex2 = liftA2 mkHex hexDig hexDig
        where mkHex a b = (a*16)+b
              hexDig = (satisfy (inRange '0' '9') <&> \c -> fromEnum c-fromEnum '0')
                       + (satisfy (inRange 'a' 'f') <&> \c -> 10+fromEnum c-fromEnum 'a')
      color = (dirArg >*> hexColor) <+? do
        nm <- dirArg
        case lookup nm colorNames of
          Just c -> return c >*> hexColor
          _ -> zero
      boolean = (True <$ several "true") <+> (False <$ several "false")

  let kw s v = v<$several s
      styleSpec :: String -> Lens' TagStyle (Maybe a) -> OpParser IO a -> OpParser IO (TagStyle -> TagStyle)
      styleSpec n l ma = several n >> nbsp >> ((Just<$>ma)<+?(Nothing<$several "none")) <&> set l
  nbsp
  stl <- styleSpec "color"     (tagColor.l'1)  color <+?
         styleSpec "bgcolor"   (tagColor.l'2)  color <+?
         styleSpec "display"   tagDisplay      (kw "block" (Block True) <+? kw "line" (Block False) <+? kw "inline" Inline) <+?
         styleSpec "underline" tagIsUnderlined boolean <+?
         styleSpec "italic"    tagIsItalic     boolean <+?
         styleSpec "bold"      tagIsBold       boolean <+?
         styleSpec "indent"    tagIndent       number <+?
         styleSpec "prefix"    tagPrefix       (quotedString '"') <+?
         styleSpec "word-wrap" tagWordWrap     number
  liftIOWarn (modifyIORef ?sessionState (style.at tag.folded %~ stl))

-- | A list of colors gotten from 'http://www.color-hex.com/color-names.html'
colorNames :: Map String String
colorNames = fromAList [
  ("alice-blue","f0f8ff"),("antique-white","faebd7"),("antique-white-1","ffefdb"),("antique-white-2","eedfcc"),
  ("antique-white-3","cdc0b0"),("antique-white-4","8b8378"),("aquamarine-1","7fffd4"),("aquamarine-2","76eec6"),
  ("aquamarine-4","458b74"),("azure-1","f0ffff"),("azure-2","e0eeee"),("azure-3","c1cdcd"),
  ("azure-4","838b8b"),("beige","f5f5dc"),("bisque-1","ffe4c4"),("bisque-2","eed5b7"),
  ("bisque-3","cdb79e"),("bisque-4","8b7d6b"),("black","000000"),("blanched-almond","ffebcd"),
  ("blue-1","0000ff"),("blue-2","0000ee"),("blue-4","00008b"),("blue-violet","8a2be2"),
  ("brown","a52a2a"),("brown-1","ff4040"),("brown-2","ee3b3b"),("brown-3","cd3333"),
  ("brown-4","8b2323"),("burlywood","deb887"),("burlywood-1","ffd39b"),("burlywood-2","eec591"),
  ("burlywood-3","cdaa7d"),("burlywood-4","8b7355"),("cadet-blue","5f9ea0"),("cadet-blue-1","98f5ff"),
  ("cadet-blue-2","8ee5ee"),("cadet-blue-3","7ac5cd"),("cadet-blue-4","53868b"),("chartreuse-1","7fff00"),
  ("chartreuse-2","76ee00"),("chartreuse-3","66cd00"),("chartreuse-4","458b00"),("chocolate","d2691e"),
  ("chocolate-1","ff7f24"),("chocolate-2","ee7621"),("chocolate-3","cd661d"),("coral","ff7f50"),
  ("coral-1","ff7256"),("coral-2","ee6a50"),("coral-3","cd5b45"),("coral-4","8b3e2f"),
  ("cornflower-blue","6495ed"),("cornsilk-1","fff8dc"),("cornsilk-2","eee8cd"),("cornsilk-3","cdc8b1"),
  ("cornsilk-4","8b8878"),("cyan-1","00ffff"),("cyan-2","00eeee"),("cyan-3","00cdcd"),
  ("cyan-4","008b8b"),("dark-goldenrod","b8860b"),("dark-goldenrod-1","ffb90f"),("dark-goldenrod-2","eead0e"),
  ("dark-goldenrod-3","cd950c"),("dark-goldenrod-4","8b6508"),("dark-green","006400"),("dark-khaki","bdb76b"),
  ("dark-olive-green","556b2f"),("dark-olive-green-1","caff70"),("dark-olive-green-2","bcee68"),("dark-olive-green-3","a2cd5a"),
  ("dark-olive-green-4","6e8b3d"),("dark-orange","ff8c00"),("dark-orange-1","ff7f00"),("dark-orange-2","ee7600"),
  ("dark-orange-3","cd6600"),("dark-orange-4","8b4500"),("dark-orchid","9932cc"),("dark-orchid-1","bf3eff"),
  ("dark-orchid-2","b23aee"),("dark-orchid-3","9a32cd"),("dark-orchid-4","68228b"),("dark-salmon","e9967a"),
  ("dark-sea-green","8fbc8f"),("dark-sea-green-1","c1ffc1"),("dark-sea-green-2","b4eeb4"),("dark-sea-green-3","9bcd9b"),
  ("dark-sea-green-4","698b69"),("dark-slate-blue","483d8b"),("dark-slate-gray","2f4f4f"),("dark-slate-gray-1","97ffff"),
  ("dark-slate-gray-2","8deeee"),("dark-slate-gray-3","79cdcd"),("dark-slate-gray-4","528b8b"),("dark-turquoise","00ced1"),
  ("dark-violet","9400d3"),("deep-pink-1","ff1493"),("deep-pink-2","ee1289"),("deep-pink-3","cd1076"),
  ("deep-pink-4","8b0a50"),("deep-sky-blue-1","00bfff"),("deep-sky-blue-2","00b2ee"),("deep-sky-blue-3","009acd"),
  ("deep-sky-blue-4","00688b"),("dim-gray","696969"),("dodger-blue-1","1e90ff"),("dodger-blue-2","1c86ee"),
  ("dodger-blue-3","1874cd"),("dodger-blue-4","104e8b"),("firebrick","b22222"),("firebrick-1","ff3030"),
  ("firebrick-2","ee2c2c"),("firebrick-3","cd2626"),("firebrick-4","8b1a1a"),("floral-white","fffaf0"),
  ("forest-green","228b22"),("gainsboro","dcdcdc"),("ghost-white","f8f8ff"),("gold-1","ffd700"),
  ("gold-2","eec900"),("gold-3","cdad00"),("gold-4","8b7500"),("goldenrod","daa520"),
  ("goldenrod-1","ffc125"),("goldenrod-2","eeb422"),("goldenrod-3","cd9b1d"),("goldenrod-4","8b6914"),
  ("gray","bebebe"),("gray-1","030303"),("gray-10","1a1a1a"),("gray-11","1c1c1c"),
  ("gray-12","1f1f1f"),("gray-13","212121"),("gray-14","242424"),("gray-15","262626"),
  ("gray-16","292929"),("gray-17","2b2b2b"),("gray-18","2e2e2e"),("gray-19","303030"),
  ("gray-2","050505"),("gray-20","333333"),("gray-21","363636"),("gray-22","383838"),
  ("gray-23","3b3b3b"),("gray-24","3d3d3d"),("gray-25","404040"),("gray-26","424242"),
  ("gray-27","454545"),("gray-28","474747"),("gray-29","4a4a4a"),("gray-3","080808"),
  ("gray-30","4d4d4d"),("gray-31","4f4f4f"),("gray-32","525252"),("gray-33","545454"),
  ("gray-34","575757"),("gray-35","595959"),("gray-36","5c5c5c"),("gray-37","5e5e5e"),
  ("gray-38","616161"),("gray-39","636363"),("gray-4","0a0a0a"),("gray-40","666666"),
  ("gray-41","696969"),("gray-42","6b6b6b"),("gray-43","6e6e6e"),("gray-44","707070"),
  ("gray-45","737373"),("gray-46","757575"),("gray-47","787878"),("gray-48","7a7a7a"),
  ("gray-49","7d7d7d"),("gray-5","0d0d0d"),("gray-50","7f7f7f"),("gray-51","828282"),
  ("gray-52","858585"),("gray-53","878787"),("gray-54","8a8a8a"),("gray-55","8c8c8c"),
  ("gray-56","8f8f8f"),("gray-57","919191"),("gray-58","949494"),("gray-59","969696"),
  ("gray-6","0f0f0f"),("gray-60","999999"),("gray-61","9c9c9c"),("gray-62","9e9e9e"),
  ("gray-63","a1a1a1"),("gray-64","a3a3a3"),("gray-65","a6a6a6"),("gray-66","a8a8a8"),
  ("gray-67","ababab"),("gray-68","adadad"),("gray-69","b0b0b0"),("gray-7","121212"),
  ("gray-70","b3b3b3"),("gray-71","b5b5b5"),("gray-72","b8b8b8"),("gray-73","bababa"),
  ("gray-74","bdbdbd"),("gray-75","bfbfbf"),("gray-76","c2c2c2"),("gray-77","c4c4c4"),
  ("gray-78","c7c7c7"),("gray-79","c9c9c9"),("gray-8","141414"),("gray-80","cccccc"),
  ("gray-81","cfcfcf"),("gray-82","d1d1d1"),("gray-83","d4d4d4"),("gray-84","d6d6d6"),
  ("gray-85","d9d9d9"),("gray-86","dbdbdb"),("gray-87","dedede"),("gray-88","e0e0e0"),
  ("gray-89","e3e3e3"),("gray-9","171717"),("gray-90","e5e5e5"),("gray-91","e8e8e8"),
  ("gray-92","ebebeb"),("gray-93","ededed"),("gray-94","f0f0f0"),("gray-95","f2f2f2"),
  ("gray-97","f7f7f7"),("gray-98","fafafa"),("gray-99","fcfcfc"),("green-1","00ff00"),
  ("green-2","00ee00"),("green-3","00cd00"),("green-4","008b00"),("green-yellow","adff2f"),
  ("honeydew-1","f0fff0"),("honeydew-2","e0eee0"),("honeydew-3","c1cdc1"),("honeydew-4","838b83"),
  ("hot-pink","ff69b4"),("hot-pink-1","ff6eb4"),("hot-pink-2","ee6aa7"),("hot-pink-3","cd6090"),
  ("hot-pink-4","8b3a62"),("indian-red","cd5c5c"),("indian-red-1","ff6a6a"),("indian-red-2","ee6363"),
  ("indian-red-3","cd5555"),("indian-red-4","8b3a3a"),("ivory-1","fffff0"),("ivory-2","eeeee0"),
  ("ivory-3","cdcdc1"),("ivory-4","8b8b83"),("khaki","f0e68c"),("khaki-1","fff68f"),
  ("khaki-2","eee685"),("khaki-3","cdc673"),("khaki-4","8b864e"),("lavender","e6e6fa"),
  ("lavender-blush-1","fff0f5"),("lavender-blush-2","eee0e5"),("lavender-blush-3","cdc1c5"),("lavender-blush-4","8b8386"),
  ("lawn-green","7cfc00"),("lemon-chiffon-1","fffacd"),("lemon-chiffon-2","eee9bf"),("lemon-chiffon-3","cdc9a5"),
  ("lemon-chiffon-4","8b8970"),("light","eedd82"),("light-blue","add8e6"),("light-blue-1","bfefff"),
  ("light-blue-2","b2dfee"),("light-blue-3","9ac0cd"),("light-blue-4","68838b"),("light-coral","f08080"),
  ("light-cyan-1","e0ffff"),("light-cyan-2","d1eeee"),("light-cyan-3","b4cdcd"),("light-cyan-4","7a8b8b"),
  ("light-goldenrod-1","ffec8b"),("light-goldenrod-2","eedc82"),("light-goldenrod-3","cdbe70"),("light-goldenrod-4","8b814c"),
  ("light-goldenrod-yellow","fafad2"),("light-gray","d3d3d3"),("light-pink","ffb6c1"),("light-pink-1","ffaeb9"),
  ("light-pink-2","eea2ad"),("light-pink-3","cd8c95"),("light-pink-4","8b5f65"),("light-salmon-1","ffa07a"),
  ("light-salmon-2","ee9572"),("light-salmon-3","cd8162"),("light-salmon-4","8b5742"),("light-sea-green","20b2aa"),
  ("light-sky-blue","87cefa"),("light-sky-blue-1","b0e2ff"),("light-sky-blue-2","a4d3ee"),("light-sky-blue-3","8db6cd"),
  ("light-sky-blue-4","607b8b"),("light-slate-blue","8470ff"),("light-slate-gray","778899"),("light-steel-blue","b0c4de"),
  ("light-steel-blue-1","cae1ff"),("light-steel-blue-2","bcd2ee"),("light-steel-blue-3","a2b5cd"),("light-steel-blue-4","6e7b8b"),
  ("light-yellow-1","ffffe0"),("light-yellow-2","eeeed1"),("light-yellow-3","cdcdb4"),("light-yellow-4","8b8b7a"),
  ("lime-green","32cd32"),("linen","faf0e6"),("magenta","ff00ff"),("magenta-2","ee00ee"),
  ("magenta-3","cd00cd"),("magenta-4","8b008b"),("maroon","b03060"),("maroon-1","ff34b3"),
  ("maroon-2","ee30a7"),("maroon-3","cd2990"),("maroon-4","8b1c62"),("medium","66cdaa"),
  ("medium-aquamarine","66cdaa"),("medium-blue","0000cd"),("medium-orchid","ba55d3"),("medium-orchid-1","e066ff"),
  ("medium-orchid-2","d15fee"),("medium-orchid-3","b452cd"),("medium-orchid-4","7a378b"),("medium-purple","9370db"),
  ("medium-purple-1","ab82ff"),("medium-purple-2","9f79ee"),("medium-purple-3","8968cd"),("medium-purple-4","5d478b"),
  ("medium-sea-green","3cb371"),("medium-slate-blue","7b68ee"),("medium-spring-green","00fa9a"),("medium-turquoise","48d1cc"),
  ("medium-violet-red","c71585"),("midnight-blue","191970"),("mint-cream","f5fffa"),("misty-rose-1","ffe4e1"),
  ("misty-rose-2","eed5d2"),("misty-rose-3","cdb7b5"),("misty-rose-4","8b7d7b"),("moccasin","ffe4b5"),
  ("navajo-white-1","ffdead"),("navajo-white-2","eecfa1"),("navajo-white-3","cdb38b"),("navajo-white-4","8b795e"),
  ("navy-blue","000080"),("old-lace","fdf5e6"),("olive-drab","6b8e23"),("olive-drab-1","c0ff3e"),
  ("olive-drab-2","b3ee3a"),("olive-drab-4","698b22"),("orange-1","ffa500"),("orange-2","ee9a00"),
  ("orange-3","cd8500"),("orange-4","8b5a00"),("orange-red-1","ff4500"),("orange-red-2","ee4000"),
  ("orange-red-3","cd3700"),("orange-red-4","8b2500"),("orchid","da70d6"),("orchid-1","ff83fa"),
  ("orchid-2","ee7ae9"),("orchid-3","cd69c9"),("orchid-4","8b4789"),("pale","db7093"),
  ("pale-goldenrod","eee8aa"),("pale-green","98fb98"),("pale-green-1","9aff9a"),("pale-green-2","90ee90"),
  ("pale-green-3","7ccd7c"),("pale-green-4","548b54"),("pale-turquoise","afeeee"),("pale-turquoise-1","bbffff"),
  ("pale-turquoise-2","aeeeee"),("pale-turquoise-3","96cdcd"),("pale-turquoise-4","668b8b"),("pale-violet-red","db7093"),
  ("pale-violet-red-1","ff82ab"),("pale-violet-red-2","ee799f"),("pale-violet-red-3","cd6889"),("pale-violet-red-4","8b475d"),
  ("papaya-whip","ffefd5"),("peach-puff-1","ffdab9"),("peach-puff-2","eecbad"),("peach-puff-3","cdaf95"),
  ("peach-puff-4","8b7765"),("pink","ffc0cb"),("pink-1","ffb5c5"),("pink-2","eea9b8"),
  ("pink-3","cd919e"),("pink-4","8b636c"),("plum","dda0dd"),("plum-1","ffbbff"),
  ("plum-2","eeaeee"),("plum-3","cd96cd"),("plum-4","8b668b"),("powder-blue","b0e0e6"),
  ("purple","a020f0"),("rebeccapurple","663399"),("purple-1","9b30ff"),("purple-2","912cee"),
  ("purple-3","7d26cd"),("purple-4","551a8b"),("red-1","ff0000"),("red-2","ee0000"),
  ("red-3","cd0000"),("red-4","8b0000"),("rosy-brown","bc8f8f"),("rosy-brown-1","ffc1c1"),
  ("rosy-brown-2","eeb4b4"),("rosy-brown-3","cd9b9b"),("rosy-brown-4","8b6969"),("royal-blue","4169e1"),
  ("royal-blue-1","4876ff"),("royal-blue-2","436eee"),("royal-blue-3","3a5fcd"),("royal-blue-4","27408b"),
  ("saddle-brown","8b4513"),("salmon","fa8072"),("salmon-1","ff8c69"),("salmon-2","ee8262"),
  ("salmon-3","cd7054"),("salmon-4","8b4c39"),("sandy-brown","f4a460"),("sea-green-1","54ff9f"),
  ("sea-green-2","4eee94"),("sea-green-3","43cd80"),("sea-green-4","2e8b57"),("seashell-1","fff5ee"),
  ("seashell-2","eee5de"),("seashell-3","cdc5bf"),("seashell-4","8b8682"),("sienna","a0522d"),
  ("sienna-1","ff8247"),("sienna-2","ee7942"),("sienna-3","cd6839"),("sienna-4","8b4726"),
  ("sky-blue","87ceeb"),("sky-blue-1","87ceff"),("sky-blue-2","7ec0ee"),("sky-blue-3","6ca6cd"),
  ("sky-blue-4","4a708b"),("slate-blue","6a5acd"),("slate-blue-1","836fff"),("slate-blue-2","7a67ee"),
  ("slate-blue-3","6959cd"),("slate-blue-4","473c8b"),("slate-gray","708090"),("slate-gray-1","c6e2ff"),
  ("slate-gray-2","b9d3ee"),("slate-gray-3","9fb6cd"),("slate-gray-4","6c7b8b"),("snow-1","fffafa"),
  ("snow-2","eee9e9"),("snow-3","cdc9c9"),("snow-4","8b8989"),("spring-green-1","00ff7f"),
  ("spring-green-2","00ee76"),("spring-green-3","00cd66"),("spring-green-4","008b45"),("steel-blue","4682b4"),
  ("steel-blue-1","63b8ff"),("steel-blue-2","5cacee"),("steel-blue-3","4f94cd"),("steel-blue-4","36648b"),
  ("tan","d2b48c"),("tan-1","ffa54f"),("tan-2","ee9a49"),("tan-3","cd853f"),
  ("tan-4","8b5a2b"),("thistle","d8bfd8"),("thistle-1","ffe1ff"),("thistle-2","eed2ee"),
  ("thistle-3","cdb5cd"),("thistle-4","8b7b8b"),("tomato-1","ff6347"),("tomato-2","ee5c42"),
  ("tomato-3","cd4f39"),("tomato-4","8b3626"),("turquoise","40e0d0"),("turquoise-1","00f5ff"),
  ("turquoise-2","00e5ee"),("turquoise-3","00c5cd"),("turquoise-4","00868b"),("violet","ee82ee"),
  ("violet-red","d02090"),("violet-red-1","ff3e96"),("violet-red-2","ee3a8c"),("violet-red-3","cd3278"),
  ("violet-red-4","8b2252"),("wheat","f5deb3"),("wheat-1","ffe7ba"),("wheat-2","eed8ae"),
  ("wheat-3","cdba96"),("wheat-4","8b7e66"),("white","ffffff"),("white-smoke","f5f5f5"),
  ("yellow-1","ffff00"),("yellow-2","eeee00"),("yellow-3","cdcd00"),("yellow-4","8b8b00"),
  ("yellow-green","9acd32")]

