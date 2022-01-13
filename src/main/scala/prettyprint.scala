package com.github.distributedclocks.tracechecker

import pprint.Tree

object prettyprint extends pprint.PPrinter {
  // we have a very specific expectation when it comes to pretty-printing `Element`s:
  // lineNumber, vectorClock, and traceId are crucial to understanding what happened / looking for more detail in the log,
  // but a regular pretty-print wouldn't even show them. this makes sure they reliably show up, sorted before any other fields
  override val additionalHandlers: PartialFunction[Any, Tree] = pprint.PPrinter.Color.additionalHandlers.orElse {
    case elem: Element =>
      pprint.Tree.Apply(
        elem.productPrefix,
        Iterator(
          pprint.Tree.KeyValue("lineNumber", treeify(elem.lineNumber, this.defaultEscapeUnicode, this.defaultShowFieldNames)),
          pprint.Tree.KeyValue("vectorClock", treeify(elem.vectorClock, this.defaultEscapeUnicode, this.defaultShowFieldNames)),
          pprint.Tree.KeyValue("traceId", treeify(elem.traceId, this.defaultEscapeUnicode, this.defaultShowFieldNames)),
        ) ++ pprint.ProductSupport.treeifyProductElements(elem, this, this.defaultEscapeUnicode, this.defaultShowFieldNames))
  }
}
