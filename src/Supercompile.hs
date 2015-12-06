import CCO.Component    (Component, component, printer, ioWrap)
import CCO.HM.Base
import CCO.HM           hiding (parser)
import CCO.Tree         (ATerm, fromTree, toTree, parser)
import Control.Arrow    (arr, (>>>))
import Debug.Trace

main = ioWrap $ parser
            >>> (component toTree :: Component ATerm Term)
            >>> arr supercompile
            >>> arr fromTree
            >>> printer
