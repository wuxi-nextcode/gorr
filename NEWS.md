# gorr 1.5.0

* Add phenotype_plot() method
* Added workflow service support
* Added queryserver support

# gorr 1.4.0

* Added *self* link to service root endpoints if not exist
* Added covariate service. See `get_covariates` and `get_covariate`

# gorr 1.3.0

* Added support for fetching playlists by name in `get_playlist`
* Added support for data.frames and tibbles in `phenotype_upload_data`

# gorr 1.2.0

* phenotype_connect added to replace gor_connect and phenotype_connect - these functions will be removed in later releases

# gorr 1.1.0

* Phenotype playlists support
* Phenotype category support
* Add get_data support for Phenotypes

# gorr 1.0.0

* Phenotype-catalog services
* Authentication headers for http requests updated
* Add support for PATCH requests
* Add support for GET queries
* Library description updated

# gorr 0.3.5

* Removed usage of deprecated dplyr methods so we don't get warning messages.
* Updated unit tests so that they don't care about the case of result headings (GOR is case-insensitive).

# gorr 0.3.4

* Handle JWT tokens that don't have a expiration date.

# gorr 0.3.3

* Misc improvements.

# gorr 0.3.2

* Persistance of query results now supported, i.e. the ability to save query results to files on the server, usually under user_data/path/to/file

# gorr 0.3.1

* Definitions can now be merged in consecutive `gor_create` calls using the replace parameter. 
* Support for custom host connections, regardless of the provided API key.

# gorr 0.3.0

* Virtual relations feature implemented, allowing referencing local data frame (via upload), and maintaining GOR create statements and definitions.
* API Error messages correctly displayed
* Fixed issue with killing remote tasks (cancelling queries)

# gorr 0.2.9

Killing queries now works as expected 

# gorr 0.2.8

Using API keys from other client-ids now works, i.e. removed hard-coding of client-id in access token request.

# gorr 0.2.7

Submitted to github
