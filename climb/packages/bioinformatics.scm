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
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system python)
  #:use-module (climb packages compression)
  #:use-module (climb packages machine-learning)
  #:use-module (climb packages python)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages java)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages tcsh))

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

(define-public canu
  (package
    (name "canu")
    (version "1.3")
    (source (origin
              (method url-fetch)
              (uri (string-append "https://github.com/marbl/canu"
                                  "/archive/v" version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0ms4b3fg744d3crbglv80dixfwkpq5iv57wbfczmaxdgkfcy91bs"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ; there are no tests
       #:phases
       (modify-phases %standard-phases
         ;; hijack configure phase to change to Makefile directory
         (replace 'configure
           (lambda _ (chdir "src")))
         ;; no "install" target
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((bin (string-append (assoc-ref outputs "out")
                                       "/bin")))
               (mkdir-p bin)
               (copy-recursively "../Linux-amd64/bin" bin)))))))
    (inputs
     `(("perl", perl)))
    (propagated-inputs
     `(("jre", icedtea-8)))
    (supported-systems '("x86_64-linux")) ;; TODO: arm support
    (home-page "https://github.com/marbl/canu")
    (synopsis "Single molecule sequence assembler for genomes large and small")
    (description
     "Canu is a fork of the Celera Assembler, designed for high-noise single-molecule
sequencing.  Canu is a hierarchical assembly pipeline which runs in four steps:
detect overlys in high-noise sequences using MHAP; generate corrected sequence consensus;
trim corrected sequences; and assemble trimmed corrected sequences.")
    (license license:gpl2)))

(define-public barrnap
  (package
    (name "barrnap")
    (version "0.7")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/tseemann/barrnap/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "16y040np76my3y82hgk4yy790smbsk4h8d60d5swlv7ha3i768gg"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (delete 'build)
         (delete 'configure)
         (add-before 'check 'do-not-use-bundled-hmmer
           (lambda* (#:key inputs #:allow-other-keys)
             ;; strip bundled HMMER
             (substitute* "bin/barrnap"
               (("\\$FindBin::RealBin/../binaries/\\$OPSYS")
                (string-append (assoc-ref inputs "hmmer") "/bin")))))
         (replace 'check
           (lambda _ (zero? (system* "make" "test" "bigtest"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (db (string-append out "/db")))
               (mkdir-p bin)
               (install-file "bin/barrnap" bin)
               (mkdir-p db)
               (copy-recursively "db" db)))))))
    (inputs
     `(("perl" ,perl)
       ("hmmer" ,hmmer)))
    (synopsis "BAsic Rapid Ribosomal RNA Predictor")
    (description "Barrnap predicts the location of ribosomal RNA genes in genomes.
It supports bacteria, archaea, mitochondria and eukaryotes.  It takes FASTA DNA sequence
as input, and write GFF3 as output.")
    (home-page "https://github.com/tseemann/barrnap")
    (license license:gpl3)))

(define-public seer
  (package
    (name "seer")
    (version "1.1.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/johnlees/seer/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0xnkz2423b84pch5583dp462lj533acyrnlqxrnh866a93871n1h"))))
    (build-system gnu-build-system)
    (arguments
     `(#:test-target "test"
       #:make-flags
       (list
        ;; PREFIX is set to $HOME by default
        (string-append "PREFIX=" (assoc-ref %outputs "out"))
        ;; override ldflags to add -lopenblas, required when armadillo is built against it
        (string-append "SEER_LDLIBS=-lhdf5 -lgzstream -lz -larmadillo"
                       " -lboost_program_options -llapack -lblas -lopenblas -lpthread"))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure))))
    (native-inputs
     `(("perl" ,perl))) ;; required for tests
    (inputs
     `(("gzstream" ,gzstream)
       ("armadillo" ,armadillo)
       ("zlib" ,zlib)
       ("boost" ,boost)
       ("hdf5" ,hdf5)
       ("dlib" ,dlib)
       ("openblas" ,openblas)
       ("lapack" ,lapack)))
    (home-page "https://github.com/johnlees/seer")
    (synopsis "Sequence element (kmer) enrichment analysis")
    (description "Sequence element enrichment analysis to determine the genetic basis
of bacterial phenotypes")
    (license license:gpl2)))

(define-public raxml
  (package
    (name "raxml")
    (version "8.2.9")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/stamatak/standard-RAxML/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1pv8p2fy67y21a9y4cm7xpvxqjwz2v4201flfjshdq1p8j52rqf7"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           ;; TODO: allow building all supported optimizations
           (lambda _ (zero? (system* "make" "-f" "Makefile.PTHREADS.gcc"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin/")))
               (mkdir-p bin)
               ;; TODO: add perl scripts
               (copy-file "raxmlHPC-PTHREADS" (string-append bin "raxmlHPC"))))))))
    (home-page "http://www.exelixis-lab.org")
    (synopsis "Maximum Likelihood based inference of large phylogenetic trees")
    (description "RAxML (Randomized Axelerated Maximum Likelihood) is a program for
sequential and parallel Maximum Likelihood based inference of large phylogenetic trees.
It can also be used for postanalyses of sets of phylogenetic trees, analyses of
alignments, and evolutionary placement of short reads.")
    (license license:gpl3)))

(define-public gubbins
  (package
    (name "gubbins")
    (version "2.1.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/sanger-pathogens/gubbins/archive/v"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "13ri69xyn5c33g41r7062d768qbdfg42xlwiil1qx1gqxdpx5n97"))))
    (build-system gnu-build-system)
    (arguments
     `(#:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'do-not-build-python
           ;; we build python frontend as a separate package below
           (lambda _ (substitute* "Makefile.am" ((" python") ""))))
         (add-before 'configure 'autoconf
           (lambda _ (zero? (system* "autoreconf" "-vif")))))))
    (inputs
     `(("zlib" ,zlib)
       ("fasttree" ,fasttree)))
    (native-inputs
     `(("pkg-config" ,pkg-config)
       ("autoconf" ,autoconf)
       ("automake" ,automake)
       ("libtool" ,libtool)
       ("check" ,check)
       ("python" ,python-3)))
    (home-page "https://sanger-pathogens.github.io/gubbins/")
    (synopsis "Rapid phylogenetic analysis of recombinant bacterial whole genome
sequences")
    (description "Gubbins (Genealogies Unbiased By recomBinations In Nucleotide Sequences)
is an algorithm that iteratively identifies loci containing elevated densities of base
substitutions while concurrently constructing a phylogeny based on the putative point
mutations outside of these regions")
    (license license:gpl2)))

(define-public python-gubbins
  ;; XXX: non-deterministic build
  (package (inherit gubbins)
           (name "python-gubbins")
           (build-system python-build-system)
           (arguments
            `(#:phases
              (modify-phases %standard-phases
                (add-after 'unpack 'change-to-python-dir
                  (lambda _ (chdir "python"))))))
           (native-inputs
            `(("python-nose" ,python-nose)
              ("raxml" ,raxml)))
           (propagated-inputs
            `(("gubbins" ,gubbins)
              ("python-biopython" ,python-biopython)
              ("python-dendropy" ,python-dendropy)
              ("python-reportlab" ,python-reportlab)
              ("python-pillow" ,python-pillow)))))

(define-public mummer
  (package
    (name "mummer")
    (version "3.23")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "mirror://sourceforge/mummer/mummer/"
                           version "/MUMmer"
                           version ".tar.gz"))
       (sha256
        (base32
         "0bv6mwqg6imgyxga24xm1cb3mfs56zba485kxgmdiq6fv3vx9yhy"))
       (modules '((guix build utils)))
       (snippet
        '(begin
           (substitute*
               (list "Makefile"
                     "scripts/Makefile")
             ;; remove hard-coded /bin/sh
             ((" /bin/sh") " sh"))
           (substitute* "Makefile"
             ;; scripts must be built separately to set correct output path
             (("kurtz tigr scripts") "kurtz tigr"))))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; only checks whether build tools are present
       #:make-flags (let ((out (assoc-ref %outputs "out")))
                      (list
                       (string-append "BIN_DIR=" (string-append out "/bin"))
                       (string-append "AUX_BIN_DIR=" (string-append out "/aux"))
                       (string-append "SCRIPT_DIR=" (string-append out "/lib"))))
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (add-before 'build 'create-bin-dir
                      (lambda* (#:key outputs #:allow-other-keys)
                        (let* ((out (assoc-ref outputs "out"))
                               (bin (string-append out "/bin"))
                               (aux (string-append out "/aux")))
                          (mkdir-p bin)
                          (mkdir-p aux))))
         (add-after 'install 'install-scripts
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (aux (string-append out "/aux"))
                    (lib (string-append out "/lib")))
               (mkdir-p lib)
               (chdir "scripts")
               (zero? (system* "make"
                               (string-append "SCRIPT_DIR=" lib)
                               (string-append "BIN_DIR=" bin)
                               (string-append "AUX_BIN_DIR=" aux)))
               (copy-file "Foundation.pm" (string-append lib "/Foundation.pm"))))))))
    (native-inputs
     `(("perl" ,perl)
       ("tcsh" ,tcsh)))
    (home-page "http://mummer.sourceforge.net")
    (synopsis "Ultra-fast alignment of large-scale DNA and protein sequences")
    (description "MUMmer is a system for rapidly aligning entire genomes, whether in
complete or draft form.")
    (license license:clarified-artistic)))

(define-public pymummer
  ;; XXX non-deterministic build
  (package
    (name "pymummer")
    (version "0.7.1")
    (source
     (origin
       (method url-fetch)
       ;; pypi does not have test data
       (uri
        (string-append
         "https://github.com/sanger-pathogens/pymummer/archive/v"
         version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0msfg89zgswx2ddl9bb3fq95006adqzps8rclc6f19099zmpmark"))))
    (build-system python-build-system)
    (native-inputs
     `(("python-nose" ,python-nose)
       ("perl" ,perl)))
    (propagated-inputs
     `(("mummer" ,mummer)
       ("fastaq" ,fastaq)))
    (home-page "https://github.com/sanger-pathogens/pymummer")
    (synopsis "Python3 module for running MUMmer and reading the output")
    (description "Python3 wrapper for running MUMmer and parsing the output.")
    (license license:gpl3)))
