xquery version "1.0-ml";

module namespace visjs = "http://leis.irad/visjs";

declare default function namespace "http://www.w3.org/2005/xpath-functions";

declare option xdmp:mapping "false";

declare function visjs:build-graph($subjects)
{
  let $subject-iris := $subjects ! sem:iri(.)
  let $edge-triples := sem:sparql(
    concat("
      PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
      PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
      SELECT DISTINCT
        ?subject
        (COALESCE(?predicateLabel, ?predicateUri) AS ?predicate)
        ?predicateUri
        ?object
      WHERE {
        {
          ?subject ?predicateUri ?object .
          FILTER( ?subject = ?subjects )
        } UNION {
          ?subject ?predicateUri ?object
          FILTER( ?object = ?revSubjects )
        }
        OPTIONAL {
          ?predicateUri rdfs:label ?predicateLabel .
        }
        ", visjs:sparql-filter(), "
        FILTER( isUri( ?object ) )
      }
    "),
    map:new((
      map:entry("subjects", $subject-iris),
      map:entry("revSubjects", $subject-iris)
    ))
  )
  let $node-uris := distinct-values((
    $edge-triples ! map:get(., "subject"),
    $edge-triples ! map:get(., "object")
  ))

  let $nodes := json:to-array(
    for $node-uri in $node-uris
    let $node-data := sem:sparql(
      concat("
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT DISTINCT
          (COALESCE(?subjectLabel, ?subject) AS ?label)
          (COALESCE(?subjectType, 'unknown') AS ?type)
        WHERE {
          OPTIONAL {
            ?subject rdfs:label ?subjectLabel .
          }
          OPTIONAL {
            ?subject rdf:type ?subjectType .
          }
        } LIMIT 1
      "),
      map:new((
        map:entry("subject", sem:iri($node-uri))
      ))
    )
    return xdmp:to-json(map:new((
      map:entry("id", $node-uri),
      map:entry("label", map:get($node-data, "label")),
      map:entry("group", map:get($node-data, "type")),
      map:entry("edgeCount", visjs:get-edge-count($node-uri))
    )))
  )

  let $edges := json:to-array(
    for $edge-data in $edge-triples
    let $id := "edge-" || map:get($edge-data, "subject") || "-" || map:get($edge-data, "object")
    let $label := xdmp:url-decode(tokenize(
      map:get($edge-data, "predicate"),
      "[/#]"
    )[last()])
    return xdmp:to-json(map:new((
      map:entry("id", $id),
      map:entry("from", map:get($edge-data, "subject")),
      map:entry("to", map:get($edge-data, "object")),
      map:entry("label", $label),
      map:entry("type", map:get($edge-data, "predicateUri"))
    )))
  )

  return
    document {
      xdmp:to-json(
        map:new((
          map:entry("nodes", $nodes),
          map:entry("edges", $edges)
        ))
      )
    }

};

declare private function visjs:get-types($subjects as xs:string*) as node()*
{
  <x>{cts:triples($subjects ! sem:iri(.), sem:iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))}</x>/*
};

declare private function visjs:retrieve-type(
  $types as node()*,
  $uri as xs:string
) as xs:string?
{
  ($types[sem:subject = $uri]/sem:object/string(), "unknown")[1]
};

declare private function visjs:get-label($subject as xs:string) as xs:string
{
  let $tokens := tokenize($subject, "/")
  return
    if (count($tokens) = 1) then $subject
    else $tokens[last() - 1] || "/" || $tokens[last()]
};

declare private function visjs:sparql-filter() as xs:string
{
  " FILTER( !( ?predicateUri = (rdfs:label, rdf:type) ) ) "
};

declare private function visjs:get-edge-count($subject) as xs:int
{

  let $params := map:new( map:entry("subject", sem:iri($subject)) )
  let $q := fn:concat(
    "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT (COUNT(DISTINCT ?object) AS ?count)
    WHERE {
      {
        ?subject ?predicateUri ?object . ",
    visjs:sparql-filter(),
    "
      }
      UNION
      {
        ?object ?predicateUri ?subject .",
        visjs:sparql-filter(),
      "}
      FILTER( ?subject != ?object)
    }
    ")
  
  let $count := 
      map:get(sem:sparql($q, $params), "count")

  return $count
};
