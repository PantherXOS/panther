(define-module (px packages gstreamer)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages qt))

;; Yields libgstqmlgl.so for qmlgl support
(define-public gst-plugins-good-qmlgl
  (package
    (inherit gst-plugins-good-qt)
    (name "gst-plugins-good-qmlgl")
    (inputs `(("gstreamer" ,gstreamer)
              ("qtbase" ,qtbase-5)
              ("qttools" ,qttools-5)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtquickcontrols2" ,qtquickcontrols2-5)
              ("qtx11extras" ,qtx11extras)
              ("qtwayland" ,qtwayland-5)
              ,@(package-inputs gst-plugins-good)))))
