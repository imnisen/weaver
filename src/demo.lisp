(defun run-server (port)
  (let* ((sock (usocket:socket-listen #(127 0 0 1) port
                                      :reuse-address t
                                      :backlog 1
                                      :element-type 'character))
         (con (usocket:socket-accept sock))
         (s (usocket:socket-stream con)))
    (let ((line (read-line s nil nil)))
      (format t "Recv from Client:[~A]~%" line))
    (format s "String from Server~%")
    (usocket:socket-close con)
    (usocket:socket-close sock)))
