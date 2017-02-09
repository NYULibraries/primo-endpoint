
Reference data:

* [Endpoint Primo now uses to collect metadata](https://ichabod.library.nyu.edu/catalog.json?page=2)
* [ Sample collection in FDA from which to collect metadata. In that case metadata in XML ](https://archive.nyu.edu/request?verb=ListRecords&metadataPrefix=oai_dc&set=hdl_2451_33605)
  
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
