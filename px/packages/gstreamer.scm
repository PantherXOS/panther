;;; Package Repository for GNU Guix
;;; Copyright © 2021-2025 Franz Geffke <mail@gofranz.com>

(define-module (px packages gstreamer)
  #:use-module ((guix licenses)
                #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix gexp)
  #:use-module (guix download)
  #:use-module (guix git-download)
  #:use-module (gnu packages gstreamer)
  #:use-module (gnu packages mp3)
  #:use-module (gnu packages qt)
  #:use-module (guix utils))

;; gst-plugins-ugly with x264 and lame (mp3) encoders enabled
(define-public gst-plugins-ugly-full
  (package
    (inherit gst-plugins-ugly)
    (name "gst-plugins-ugly-full")
    (arguments
     (substitute-keyword-arguments (package-arguments gst-plugins-ugly)
       ((#:configure-flags flags #~'())
        #~(cons* "-Dx264=enabled"
                 "-Dgpl=enabled"
                 #$flags))))
    (inputs
     (modify-inputs (package-inputs gst-plugins-ugly)
       (append lame)))))

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
