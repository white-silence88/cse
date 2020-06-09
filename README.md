# Chainsaw Service Engine (CSE)
New framework on Common Lisp for quick development web servers for API.
## Dependencies
* woo - fast async web server on CommonLisp (based in libev)
* cl-async - library for work with async code
* alexandria - library with some sugar procedure
* bt-semaphore - library for work with threads
* jonathan - library for work  (read) with JSON
* cl-ppcre - library for work with strings
## Commands examples
* (cse:application->start "http") - run http server thread
* (cse:application->kill "http") - kill http server thread
* (cse:application->get/threads) - get list of threads
* (cse:application->get/thread-by-name "http") - get thread by name
* (cse:application->info/threads) - get list of active threads
* (cse:json-file->>tree "/example/file.json") - get alist from JSON
* (cse:json-file->>routes-map "example/map.json") - get routes map (alist) from JSON
* (cse:routes-map/find routes-map "/api/example") - get pair (url-pattern and config HashTable) from routes map
