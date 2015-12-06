import CCO.Component    (Component, component, printer, ioWrap)
import CCO.HM           (Term)
import CCO.Tree         (ATerm, toTree, parser)
import Control.Arrow    (arr, (>>>))

main = ioWrap $
       parser >>> (component toTree :: Component ATerm Term) >>> printer
