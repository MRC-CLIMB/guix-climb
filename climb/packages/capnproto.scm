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

(define-module (climb packages capnproto)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system gnu))

(define-public capnproto
  (package
    (name "capnproto")
    (version "0.5.3")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://capnproto.org/capnproto-c++-"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "1yvaadhgakskqq5wpv53hd6fc3pp17mrdldw4i5cvgck4iwprcfd"))
              (modules '((guix build utils)))
              (snippet
               '(begin
                  (substitute* "src/kj/async-io-test.c++"
                    ;; "fix" test which uses /etc/services to resolve port
                    ((":http") ":80"))))))
    (build-system gnu-build-system)
    (home-page "https://capnproto.org/")
    (synopsis "Cap'n Proto cerealization protocol")
    (description "Cap’n Proto is an insanely fast data interchange format and
capability-based RPC system.  Think JSON, except binary.  Or think Protocol
Buffers, except faster.")
    (license license:expat)))
