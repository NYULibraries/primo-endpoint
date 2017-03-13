# Primo Endpoint

Configurable metadata aggregator and crosswalk for NYU Libraries collections designed to populate Primo.
Can run as a web server and dynamically update document cache.

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
