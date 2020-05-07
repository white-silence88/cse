# Chainsaw Service Engine (CSE)
New generation of web-service framework writen on Common Lisp.
## Dependencies
* woo
* cl-async
* alexandria
* bt-semaphore
## Commands examples
* (cse:application->start "http") - run http server thread
* (cse:application->kill "http") - kill http server thread
* (cse:application->get/threads) - get list of threads
* (cse:application->get/thread-by-name "http") - get thread by name
* (cse:application->info/threads) - get list of active threads
