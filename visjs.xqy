xquery version "1.0-ml";

module namespace resource = "http://marklogic.com/rest-api/resource/visjs";

import module namespace kl = "http://leis.irad/visjs" at "/ext/mlpm_modules/visjs-graph/visjs-lib.xqy";

import module namespace json = "http://marklogic.com/xdmp/json" at "/MarkLogic/json/json.xqy";
import module namespace sem = "http://marklogic.com/semantics" at "/MarkLogic/semantics.xqy";

declare namespace rapi = "http://marklogic.com/rest-api";

(:
declare %rapi:transaction-mode("update") function post(
  $context as map:map,
  $params  as map:map,
  $input   as document-node()*
  ) as document-node()*
{
  let $as-xml := json:transform-from-json($input)

  return insert-triple($as-xml)

};

declare function insert-triple($as-xml as node()) {

  let $s := $as-xml//*:s/data()
  let $p := $as-xml//*:p/string()
  let $o := $as-xml//*:o/string()

  let $triple :=
    element sem:triple {
      element sem:subject {$s},
      element sem:predicate {$p},
      element sem:object {$o}
    }

  let $_ :=
    if(fn:doc-available($s)) then
      xdmp:node-insert-child(fn:doc($s)/*, $triple)
    else if(fn:doc-available($o)) then
      xdmp:node-insert-child(fn:doc($o)/*, $triple)
    else
      sem:rdf-insert(sem:triple(sem:iri($s), $p, $o))

  let $_ :=  xdmp:log("Inserted new triple for " || $s)
  return ()
};:)


declare function get(
  $context as map:map,
  $params  as map:map
) as document-node()*
{
  let $subject := map:get($params, "subject")
  return kl:build-graph($subject, map:get($params, "expand") = "true")
};

