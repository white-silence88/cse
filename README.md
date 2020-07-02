# Chainsaw Service Engine (CSE)
New framework on Common Lisp for quick development web servers for API.
## Dependencies
* woo - fast async web server on CommonLisp (based in libev)
* cl-async - library for work with async code
* alexandria - library with some sugar procedure
* bt-semaphore - library for work with threads
* jonathan - library for work  (read) with JSON
* cl-ppcre - library for work with strings
## CSE API
### Work with server application
#### application->start
Procedure for run HTTP server thread
```common-lisp
(cse:application->start "http")
```
#### application->kill
Procedure for kill HTTP server
```common-lisp
(cse:application->kill "http")
```
#### application->get/threads
Procedure for get list of server threads
```common-lisp
(cse:application->get/threads)
```
#### application->get/thread-by-name
Procedure for get application thread by thread name
```common-lisp
(cse:application->get/thread-by-name "http")
```
#### application->info/threads
Procedure for get info about all application threads
```common-lisp
(cse:application->info/threads)
```
### Work with JSON & .json files
#### json-file->>tree
Procedure for get simple tree config from .json file
```common-lisp
(cse:json-file->>tree "/example/file.json")
```
#### json-file->>routes-map
Procedure fro get routes map config from .json file
```common-lisp
(cse:json-file->>routes-map "example/map.json")
```
#### jsonify 
Procedure for convert alist to JSON string
```common-lisp
(cse:jsonify alist) - get JSON from alist
```
### Work with routes
#### routes-config->>routes-map
Procedure for get list with routes pairs (pattern & HashTable config).
```common-lisp
(cse:routes-config->>routes-map tree-from-file)
```
#### routes-map/find
Procedure get pair (url-pattern and config HashTable) from routes map
```common-lisp
(cse:routes-map/find routes-map "/api/example")
```
### Work with Woo server
#### woo/env->>request
Procedure for get config list from Woo server request env data
```common-lisp
(cse:woo/env->>request woo-env-value)
```
### Procedure, for work with answers
#### seon-answers/success
Procedure for get valid answer on simple JSON format by SEON standard (based on JSON API).
#### seon-anwers/errors
Procedure for get valid error answer on simple JSON format by SEON standard (based on JSON API).
#### seon-answers/errors->not-found
Procedure for get error 404.
