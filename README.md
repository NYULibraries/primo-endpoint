# Primo Endpoint

[![Build Status](https://travis-ci.org/NYULibraries/primo-endpoint.svg?branch=master)](https://travis-ci.org/NYULibraries/primo-endpoint)

Configurable metadata aggregator and crosswalk for NYU Libraries collections designed to populate Primo.
Can run as a web server and dynamically update document cache.

## Production

```
> docker build -t primo-endpoint .
> docker run -p 80 primo-endpoint
```

Logs to stdout by default.  Startup can be optimized by persisting the /cache volume.

## Development

### Installation

```
> curl -sSL https://get.haskellstack.org/ | sh
> stack install
```

### Usage:

```
Usage: primo-endpoint [OPTION...]
  -c FILE   --config=FILE        Load configuration from FILE [config.yml]
  -a FILE   --auth=FILE          Load auth rules from FILE [auth.yml]
  -C DIR    --cache=DIR          Use DIR for cache files [$XDR_CACHE_DIR/primo-endpoint]
  -f        --force              Force an initial update of all collections
  -o[DEST]  --output[=DEST]      Write JSON output to file [-]
  -w[PORT]  --web-server[=PORT]  Run a web server on PORT [80] to serve the result
  -l        --log-access         Log access to stdout
  -v        --verbose            Log collection refreshes to stdout
```

### Config

The configuration is read from a YAML (or JSON) file with the following structure:

* `interval`: number of seconds for which to cache collections before reloading (by default)
* `fda`: FDA-specific configuration options:
    * `collections`: maximum number of collections to load from index to use in translating `hdl`s to `id`s
* `generators`: a set of named generator "macro" functions that can be used as generator keys, substituting passed object arguments for input fields
* `templates`: a set of named field generator templates, each of which contains a set of field generators
* `collections`: a set of named collections, each with the following fields:
    * `source`: a source type (see below), which may also take additional arguments on the collection object
    * `template`: optional string or array of 0 or more templates (referencing names in the `templates` object), which are all unioned together
    * `fields`: additional local "custom" generator fields for this collection

See `config.yml` for an example.

#### Sources

Each collection can have one of the following source values to specify the endpoint to pull from:

* FDA: `https://archive.nyu.edu/rest/collections/$id`
  requires `id` (internal) or `hdl` (suffix)
* DLTS: `http://discovery.dlib.nyu.edu:8080/solr3_discovery/$core/select`
  requires `core` (`core` (none), `viewer`, or `nyupress`) and `code` (collection code)
* DLib: `http://dlib.nyu.edu/$path`
  requires `path`
* SDR: `https://geo.nyu.edu/catalog`
* SpecialCollections: `https://specialcollections.library.nyu.edu/search/catalog.json`
  requires `filters` object mapping field to value
* JSON: raw JSON file with array of documents in native key-value format;
  requires `file` or `url`; mainly for testing purposes

#### Fields

Field definitions are made up of the following:

* Object with one or more key-value pairs, applied in the following order (highest to lowest precedence):
    * Single fields, which are processed independently and then combined (as if in an array):
        * `field`: name of source field to copy
        * `string`: string literal to create single value
        * `paste`: list of definitions, or string with `$field` or `${field}` placeholders to substitute (`$$` for a literal `$`); the resulting strings are pasted together (no delimiter) as a cross-product (so the number of resulting values is the product of the number of values from each element)
        * `handle`: definition. Convert a string of the form "http://hdl.handle.net/XXX/YYY.ZZZ" to "hdl-handle-net-XXX-YYY-ZZZ".  Any non-matching input is discarded.
        * `value`: any definition (for convenient nesting)
        * generator name: key-definition arguments as object. Substitutes a generator "macro" from the generator section, assigning the given keys to their corresponding values as input fields to the macro.  The generator can also see any other input fields as well.
    * Post-processors that first process the rest of the definition, and then apply a transformation on the result:
        * `date`: string [strptime format](http://hackage.haskell.org/package/time/docs/Data-Time-Format.html).  Tries to parse each value in the result with the given format and produces a timestamp in standard format (relevant prefix of "%Y-%m-%dT%H:%M:%S%QZ") as output. Any inputs that cannot be parsed are discarded.
        * `lookup`: key-definition lookup table as object. Applies the lookup table translation to each input, substituting the right-side definition for any matching left-side key. If no key matches, the input is discarded.
        * `limit`: integer. Take only the first *n* values from the input, discarding the rest.
        * `default`: definition. If there are no produced input values, provide the definition instead.
        * `join`: string literal delimiter. Paste all the inputs together, separated by the given delimiter.  Always produces exactly one output.
* Array: all produced values are merged, producing the sum of all the input values.
* String literal containing only `.`, `_`, and alphanumerics: passed to `field`
* Any other string literal: passed to `paste`
* Null: same as empty array (produces 0 values)

There are two special input fields added to every source document:

* `_key`: The collection key
* `_name`: The collection name field

## Reference data:

* [Endpoint Primo now uses to collect metadata](https://ichabod.library.nyu.edu/catalog.json?page=2)
* [ Sample collection in FDA from which to collect metadata. In that case metadata in XML ](https://archive.nyu.edu/request?verb=ListRecords&metadataPrefix=oai_dc&set=hdl_2451_33605)
* [REST endpoint for FDA collection](https://archive.nyu.edu/rest/collections/631/items?expand=metadata,parentCollection)
* [Ichabod FDA source reader](https://github.com/NYULibraries/ichabod/blob/development/lib/ichabod/resource_set/source_readers/fda_collection_rest_reader.rb)
* [DLTS Freedom collection](http://discovery.dlib.nyu.edu:8080/solr3_discovery/viewer/select?fq=sm_collection_code:fdm&wt=json)
  
  Mapping to NYUCore for that collection:

  identifier: ["identifier.uri","identifier.citation"]
  title: ["title" ]
  creator: ["contributor.author" ]
  description: ["description" ]
  date: ["date.issued" ]
  publisher: ["publisher.place:publisher,date.issued"]
  format: ["format" ]
  rights: ["rights" ]
  subject: ["subject" ]
  relation: ["identifier.citation"]

## Required fields for primo

* "id": for FDA, "fda:hdl-handle-net-2451-XXXX"
* "desc_metadata__addinfolink_tesim"
* "desc_metadata__addinfotext_tesim"
* "desc_metadata__available_tesim"
* "desc_metadata__citation_tesim"
* "desc_metadata__creator_tesim"
* "desc_metadata__data_provider_tesim"
* "desc_metadata__date_tesim"
* "desc_metadata__description_tesim"
* "desc_metadata__edition_tesim"
* "desc_metadata__format_tesim"
* "desc_metadata__isbn_tesim"
* "desc_metadata__language_tesim"
* "desc_metadata__location_tesim"
* "desc_metadata__publisher_tesim"
* "desc_metadata__relation_tesim"
* "desc_metadata__repo_tesim"
* "desc_metadata__resource_set_tesim"
* "desc_metadata__restrictions_tesim"
* "desc_metadata__rights_tesim"
* "desc_metadata__series_tesim"
* "desc_metadata__subject_tesim"
* "desc_metadata__subject_spatial_tesim"
* "desc_metadata__subject_temporal_tesim"
* "desc_metadata__title_tesim"
* "desc_metadata__type_tesim"
* "desc_metadata__version_tesim"
* "collection_ssm"
