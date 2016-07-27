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

(define-module (climb packages bioinformatics)
  #:use-module ((guix licenses) #:prefix license:)
  #:use-module (guix packages)
  #:use-module (guix download)
  #:use-module (guix build-system python)
  #:use-module (gnu packages)
  #:use-module (gnu packages python))

(define-public fastaq
  (package
    (name "fastaq")
    (version "3.12.1")
    (source
     (origin
       (method url-fetch)
       ;; Pypi does not have test data
       (uri
        (string-append
         "https://github.com/sanger-pathogens/Fastaq/archive/v"
         version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1y02gm1sk6hwgzmal9kwrpwnsmykk58337079ar5i7vdjnrb5vyy"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)))
    (home-page "https://github.com/sanger-pathogens/Fastaq")
    (synopsis "Python tools to manipulate FASTA and FASTQ (and other format) files")
    (description
     "Fastaq is a Python script and library to manipulate genetic data.  Currently
supported input formats are: FASTA, FASTQ, GFF3, EMBL,GBK, Phylip; optionally gzipped.")
    (license license:gpl3)))
