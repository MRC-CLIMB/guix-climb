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

(define-module (climb packages formatting)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system cmake))
  ;;#:use-module (gnu packages))

(define-public fmt
  (package
    (name "fmt")
    (version "3.0.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
              "https://github.com/fmtlib/fmt/archive/"
              version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "18cn3hi9fmr31lqprzlx0fgs5wlnkfl33f96plld0rl1b0jxnxgh"))))
    (build-system cmake-build-system)
    (home-page "http://fmtlib.net/latest/index.html")
    (synopsis "Small, safe and fast formatting library")
    (description "fmt is a formatting library for C++.  It can be used
as a safe alternative to printf or as a fast alternative to IOStreams.")
    (license license:bsd-2)))
