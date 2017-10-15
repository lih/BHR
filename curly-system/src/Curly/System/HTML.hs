module Curly.System.HTML where

import Definitive
import Curly.System.Base
import Curly.Core
import Curly.Core.Library
import Language.Format

system = System "html" id (void . rawProgram [TextSection]) Nothing $ RawSystem (yb bytesBuilder . foldMap encode . htmlDoc . catMain . genLeaf (c'set zero))
  where htmlDoc d = c'string $ format (fold ["<!DOCTYPE html>"
                                            ,"<html><head><link type='text/css' rel='stylesheet' href='curly.css' />"
                                            ,"<script type='text/javascript' src='curly.js'></script>"
                                            ,"<meta http-equiv='Content-Type' content='text/html; charset=utf-8' />"
                                            ,"</head><body>%s</body></html>"]) d
        catMain (e,es) = c'string $ format "<div id='main'>%s</div><div id='depends'>%s</div>" e (fold es)
        htmlQuote s = foldMap quoteChar s
          where quoteChar '<' = "&lt;"
                quoteChar '>' = "&gt;"
                quoteChar '&' = "&amp;"
                quoteChar '\'' = "&apos;"
                quoteChar '"' = "&quot;"
                quoteChar c = [c]
        genLeaf s e = (format "<div class='leaf'><pre class='type'>%s</pre><div class='value'>%s</div><div class='doc'>%s</div></div>" (htmlQuote t) v d
                      ,vs+ds)
          where (v,vs) = genExpr s (e^.leafVal)
                (d,ds) = genDoc (e^.leafDoc)
                t = show (e^.leafType)
        genDoc (Join (DocTag t as b)) = (format "<span class='doc-%s'%s>%s</span>" t
                                         (c'string $ foldMap (\(a,v) -> format " %s=%s" a v) as)
                                         (intercalate " " (map (fst . genDoc) b)),[])
        genDoc (Pure v) = (v,[])
        genExpr s e = case sem e of
          SemSymbol (iD@(GlobalID n i),e') -> (
            maybe n (\(n',l) -> format "<a href='#%s-%s'>%s</a>" (show l) n' n) (guard (has t'Join e') >> i)
            ,case e' of
            Pure _ -> []
            Join e' -> let s' = touch iD s
                       in maybe [] (\(n,l) -> [format "<h1 id='%s-%s'>%s</h1>" (show l) n n]) i
                          + uncurry (:) (maybe (genExpr s' e') (genLeaf s')
                                         (i >>= \(n,l) -> findLib l >>= \fl -> fl^.flLibrary.symbols.at n)))
          SemAbstract (GlobalID n _) e ->
            let (e',es) = genExpr s e
            in (format "<div class='lambda'><span class='param'>%s</span><div class='body'>%s</div></div>" n e'
               ,es)
          SemApply f x ->
            let (f',fs) = genExpr s f
                (x',xs) = genExpr s x
            in (format "<span class='apply'><span class='function'>%s</span>&nbsp;<span class='argument'>%s</span></span>" f' x',fs+xs)
