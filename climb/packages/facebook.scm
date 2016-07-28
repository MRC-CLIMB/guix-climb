;;; GNU Guix --- Functional package management for GNU
;;; Copyright © 2016 Marius Bakke <m.bakke@warwick.ac.uk>
;;;
;;; This file is NOT part of GNU Guix, but is supposed to be used with GNU
;;; Guix and thus has the same license.
;;;
;;; GNU Guix is free software; you can redistribute it and/or modify it
;;; under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 3 of the License, or (at
;;; your option) any later version.
;;;
;;; GNU Guix is distributed in the hope that it will be useful, but
;;; WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with GNU Guix.  If not, see <http://www.gnu.org/licenses/>.

(define-module (climb packages facebook)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages libevent)
  #:use-module (gnu packages logging)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages popt)
  #:use-module (gnu packages python)
  #:use-module (gnu packages ruby)
  #:use-module (gnu packages tls))

(define-public folly
  (package
    (name "folly")
    (version "2016.07.26")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/facebook/folly/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "043zq5q8bzs9b6i7yfnzxl72q56gycymizjajvnpqhf633hidgg0"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; TODO requires copying in googletest sources
       #:configure-flags (list (string-append
                                "--with-boost-libdir="
                                (assoc-ref %build-inputs "boost") "/lib"))
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'change-to-build-dir
           (lambda _ (chdir "folly")))
         (add-before 'configure 'autoreconf
           (lambda _ (zero? (system* "autoreconf" "-vif"))))
               )))
    (inputs
     `(("boost" ,boost)
       ("double-conversion" ,double-conversion)
       ("gflags" ,gflags)
       ("libevent" ,libevent)
       ("openssl" ,openssl)))
    (propagated-inputs
     `(("glog" ,glog)))
    (native-inputs
     `(("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("pkg-config" ,pkg-config)
       ("python" ,python)
       ("ruby" ,ruby)))
    (home-page "https://github.com/facebook/folly")
    (synopsis "Library of C++11 components designed with practicality and efficiency in mind")
    (description "Folly (acronymed loosely after Facebook Open Source Library) complements
Boost, std and other C++ libraries through a diverse collection of tools and wrappers.")
    (license license:asl2.0)))
