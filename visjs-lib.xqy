xquery version "1.0-ml";

module namespace visjs = "http://leis.irad/visjs";

declare function build-graph(
  $subjects,
  $is-expand as xs:boolean
  )
{
  let $nodes := json:array()
  let $links := json:array()
  let $nodes-map := map:map()

  let $subject-labels := <x>{cts:triples($subjects ! sem:iri(.), sem:iri("http://www.w3.org/2000/01/rdf-schema#label"))}</x>/*
  let $subject-types := <x>{cts:triples($subjects ! sem:iri(.), sem:iri("http://www.w3.org/1999/02/22-rdf-syntax-ns#type"))}</x>/*

  let $_ :=
  for $subject in $subjects

  let $params := map:new((
    map:entry("subject", sem:iri($subject))
  ))
  let $q := "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT (COALESCE(?predicateLabel, ?predicateUri) AS ?predicate)
           ?object
           ?label
    WHERE {
      ?subject ?predicateUri ?object .
      FILTER(?predicateUri != rdfs:label &amp;&amp; ?predicateUri != rdf:type)
      OPTIONAL {
        ?predicateUri rdfs:label ?predicateLabel .
      }
      OPTIONAL {
        ?object rdfs:label ?label .
      }
    }
    LIMIT 100
  "
  
  let $results := sem:sparql($q, $params)

  (: Do separate query for triples where our uri is the object. :)
  let $params := map:new((
    map:entry("object", sem:iri($subject))
  ))
  let $q := "
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    SELECT ?subject
           (COALESCE(?predicateLabel, ?predicateUri) AS ?predicate)
           ?label
    WHERE {
      ?subject ?predicateUri ?object .
      OPTIONAL {
        ?subject rdfs:label ?label .
      }
      OPTIONAL {
        ?predicateUri rdfs:label ?predicateLabel .
      }
    }
    LIMIT 100
  "

  let $results-obj := sem:sparql($q, $params)

  return
        (
          if ($is-expand) then ()
          else
            let $node := json:object()
            let $label := ($subject-labels[sem:subject = $subject]/sem:object/string(), $subject)[1]
            let $type := ($subject-types[sem:subject = $subject]/sem:object/string(), "unknown")[1]
            return (
              map:put($node, "label", $label),
              map:put($node, "id", $subject),
              map:put($node, "shape", "image"),
              map:put($node, "type", $type),
              map:put($node, "linkCount", get-link-count($subject)),
              map:put($node, "image", get-icon($subject)),
              map:put($node, "color", get-node-color()),
              map:put($node, "g", build-glyphs($subject)),
              map:put($nodes-map, $subject, $node)
            ),

          for $result in $results
          let $object := map:get($result, "object")
          let $node := json:object()
          return (
            map:put($node, "label", (map:get($result, "label"),$object)[1]),
            map:put($node, "id", $object),
            map:put($node, "type", "node"),
            map:put($node, "shape", "image"),
            map:put($node, "linkCount", get-link-count($object)),
            map:put($node, "image", get-icon($object cast as xs:string)),
            map:put($node, "color", get-node-color()),
            map:put($node, "g", build-glyphs($object)),
            map:put($nodes-map, $object, $node),

            let $link := json:object()
            where $subject != $object cast as xs:string
            return (
              map:put($link, "id", "link-" || $subject || "-" || $object),
              map:put($link, "from", $subject),
              map:put($link, "to", $object),
              let $predicate := map:get($result, "predicate")
              let $predicate := (fn:tokenize($predicate, "#"), $predicate)[2] (: use the string after #, if any :)
              let $predicate := xdmp:url-decode($predicate)
              return map:put($link, "label", $predicate),
              map:put($link, "type", "link"),
              map:put($link, "color", get-link-color()),
              map:put($link, "c", "#860082"),
              map:put($link, "w", "4"),
              map:put($link, "fb", "true"),
              json:array-push($links, $link)
            )
          ),

          for $result in $results-obj
          let $subj := map:get($result, "subject")
          let $node := json:object()
          return (
            map:put($node, "label", (map:get($result, "label"),$subj)[1]),
            map:put($node, "id", $subj),
            map:put($node, "type", "node"),
            map:put($node, "shape", "image"),
            map:put($node, "linkCount", get-link-count($subj)),
            map:put($node, "image", get-icon($subj cast as xs:string)),
            map:put($node, "color", get-node-color()),
            map:put($node, "g", build-glyphs($subj)),
            map:put($nodes-map, $subj, $node),

            let $link := json:object()
            where $subject != $subj cast as xs:string
            return (
              map:put($link, "id", "link-" || $subj || "-" || $subject),
              map:put($link, "from", $subj),
              map:put($link, "to", $subject),
              let $predicate := map:get($result, "predicate")
              let $predicate := (fn:tokenize($predicate, "#"), $predicate)[2] (: use the string after #, if any :)
              let $predicate := xdmp:url-decode($predicate)
              return map:put($link, "label", $predicate),
              map:put($link, "type", "link"),
              map:put($link, "color", get-link-color()),
              map:put($link, "c", "#860082"),
              map:put($link, "w", "4"),
              map:put($link, "fb", "true"),
              json:array-push($links, $link)
            )
          )
        )

  (: Move node data from map to array. :)
  let $_ :=
    for $key in map:keys($nodes-map)
    return
      json:array-push($nodes, map:get($nodes-map, $key))

  return
    document {
      xdmp:to-json(
        let $data-object := json:object()
        let $_ := map:put($data-object, "nodes", $nodes)
        let $_ := map:put($data-object, "links", $links)
        return $data-object
      )
    }

};

declare private function get-icon($subject as xs:string) as xs:string
{
  "bower_components/ml-visjs-graph-ng/dist/images/generic.png"
};

declare private function get-node-color() as json:object
{
  let $color-node := json:object()
  let $highlight-node := json:object()
  let $_ := (
    map:put($highlight-node, "background", "white"),
    map:put($highlight-node, "border", "#860082"),
    map:put($color-node, "highlight", $highlight-node),
    map:put($color-node, "background", "white"),
    map:put($color-node, "border", "black")
  )

  return $color-node
};

declare private function get-link-color() as json:object
{
  let $color-node := json:object()
  let $_ := map:put($color-node, "color", "#860082")
  let $_ := map:put($color-node, "highlight", "#860082")

  return $color-node
};

declare private function get-label($subject as xs:string) as xs:string
{
  let $tokens := tokenize($subject, "/")
  let $_ := xdmp:log("Token count: " || count($tokens))
  return
    if (count($tokens) = 1) then $subject
    else $tokens[last() - 1] || "/" || $tokens[last()]
};

declare private function build-glyphs($subject) as json:array?
{
  let $nw := map:new((
    map:entry("t", "+"),
    map:entry("c", "#828600"),
    map:entry("p", "nw")
  ))
  let $count :=
    if(sem:isIRI($subject)) then
      count(cts:triples(sem:iri($subject))) + count(cts:triples((), (), sem:iri($subject)))
    else
      0
  where $count > 0
  return
    let $ne := map:new((
      map:entry("c", "#86003f"),
      map:entry("p", "ne"),
      map:entry("t", if ($count lt 101) then string($count) else "100+")
    ))
    let $arr := json:array()
    return (
      json:array-push($arr, $ne),
      json:array-push($arr, $nw),
      $arr
    )
};

declare private function get-link-count($subject) as xs:string
{

  let $params := map:new( map:entry("subject", sem:iri($subject)) )
  let $q := "
    SELECT DISTINCT ?object
    WHERE {
      {
        ?subject ?predicate ?object .
        FILTER(regex(?object, ('/incident/', '/person/', '/bulletin/', '/vehicle/')))
      }
      UNION
      {
        ?object ?predicate ?subject
        FILTER(regex(?object, ('/incident/', '/person/', '/bulletin/', '/vehicle/')))
      }
    }
    LIMIT 100
  "
  
  let $count := 
    if (sem:isIRI($subject)) then
      count( sem:sparql($q, $params) )
    else
      0

  return string($count)
};
