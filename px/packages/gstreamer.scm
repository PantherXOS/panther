(define-module (px packages gstreamer)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages qt))

(define-public gst-plugins-good-qmlgl
  (package
    (inherit gst-plugins-good)
    (name "gst-plugins-good-qmlgl")
    (inputs `(("qtbase" ,qtbase-5)
              ("qtdeclarative" ,qtdeclarative-5)
              ("qtquickcontrols2" ,qtquickcontrols2-5)
              ("qtx11extras" ,qtx11extras)
              ("qtwayland" ,qtwayland-5)
              ,@(package-inputs gst-plugins-good)))))
