#lang racket 

(require web-server/servlet
         web-server/servlet-env)

(define (hello-servlet req)
  (response
   200                 ; response code
   #"OK"               ; response message
   (current-seconds)   ; timestamp
   TEXT/HTML-MIME-TYPE ; MIME type for content
   '()                 ; additional headers

   ; the following parameter accepts the output port
   ; to which the page should be rendered.
   (λ (client-out)
     (write-string "Hello, world!" client-out))))
  
; start serving:
(serve/servlet hello-servlet)