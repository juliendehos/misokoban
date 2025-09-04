
import Miso

import Component

main :: IO ()
main = run $ startComponent mkComponent

#ifdef WASM
foreign export javascript "hs_start" main :: IO ()
#endif

