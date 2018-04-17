(in-package :weaver)

(defclass response ()
  ((content-type :reader content-type)
   (headers-out :reader headers-out)
   (status-code :reader status-code)
   (content :reader response-content :initarg :content)))
