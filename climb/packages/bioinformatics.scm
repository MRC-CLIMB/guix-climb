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
  #:use-module (guix git-download)
  #:use-module (guix build-system ant)
  #:use-module (guix build-system gnu)
  #:use-module (guix build-system perl)
  #:use-module (guix build-system python)
  #:use-module (climb packages python)
  #:use-module (climb packages serialization)
  #:use-module (climb packages xml)
  #:use-module (gnu packages)
  #:use-module (gnu packages autotools)
  #:use-module (gnu packages bioinformatics)
  #:use-module (gnu packages bash)
  #:use-module (gnu packages boost)
  #:use-module (gnu packages check)
  #:use-module (gnu packages compression)
  #:use-module (gnu packages haskell)
  #:use-module (gnu packages imagemagick)
  #:use-module (gnu packages java)
  #:use-module (gnu packages machine-learning)
  #:use-module (gnu packages maths)
  #:use-module (gnu packages mpi)
  #:use-module (gnu packages parallel)
  #:use-module (gnu packages pdf)
  #:use-module (gnu packages perl)
  #:use-module (gnu packages pkg-config)
  #:use-module (gnu packages python)
  #:use-module (gnu packages serialization)
  #:use-module (gnu packages statistics)
  #:use-module (gnu packages shells)
  #:use-module (gnu packages swig)
  #:use-module (gnu packages xml)
  #:use-module (gnu packages zip))

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
    (arguments
     `(#:configure-flags '("--single-version-externally-managed"
                           "--root=/"))) ;; avoid making eggs
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
         "1pv8p2fy67y21a9y4cm7xpvxqjwz2v4201flfjshdq1p8j52rqf7"))
       (modules '((guix build utils)))
       (snippet
        ;; Delete 18MB of bundled windows executables.
        '(for-each delete-file-recursively
                   (find-files "." "^WindowsExecutables.*"
                               #:directories? #t)))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (replace 'build
           ;; RAxML ships with a separate Makefile for each optimization.
           ;; We simply build all of them and add to the default ouput.
           (lambda _
             (for-each (lambda (makefile)
                         ;; Parallel build not supported.
                         (zero? (system* "make" "-j" "1" "-f" makefile)))
                       (find-files "." "^Makefile.*\\.gcc$"))))
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/share/doc")))
               (mkdir-p doc)
               (copy-recursively "manual" (string-append doc "/RAxML"))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "." "^raxmlHPC.*"))
               (for-each (lambda (file)
                           (install-file file bin))
                         (find-files "usefulScripts" ".*"))))))))
    (native-inputs
     `(("mpi" ,openmpi)
       ("perl" ,perl)))
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
            `(#:configure-flags '("--single-version-externally-managed"
                                  "--root=/") ;; avoid making eggs
              ;; TODO: even with the below raxml-AVX patch, there is still one
              ;; AssertionError on test_external_dependancies.py:82
              ;; Disable for now. It was working before commit b022191.
              #:tests? #f
              #:phases
              (modify-phases %standard-phases
                (add-after 'unpack 'do-not-use-raxml-avx
                  ;; Some tests fail with raxmlHPC-AVX, but not other versions.
                  ;; So prevent pygubbins from selecting it.
                  ;; see https://github.com/stamatak/standard-RAxML/issues/39
                  (lambda _ (substitute* "python/gubbins/RAxMLExecutable.py"
                              (("'raxmlHPC-AVX',") ""))))
                (add-before 'patch-source-shebangs 'change-to-python-dir
                  (lambda _ (chdir "python")))
                (add-after 'install 'wrap-program
                  (lambda* (#:key inputs outputs #:allow-other-keys)
                    (let ((out (assoc-ref outputs "out"))
                          (path (map (lambda (pkg)
                                       (string-append (assoc-ref inputs pkg) "/bin"))
                                     '("gubbins" "raxml"))))
                      ;; tell run_gubbins.py where gubbins and raxml is
                      (wrap-program (string-append out "/bin/run_gubbins.py")
                        `("PATH" ":" prefix (,(string-join path ":"))))))))))
           (native-inputs
            `(("python-nose" ,python-nose)))
           (propagated-inputs
            `(("gubbins" ,gubbins)
              ("raxml" ,raxml)
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
    (arguments
     `(#:configure-flags '("--single-version-externally-managed"
                           "--root=/"))) ;; avoid making eggs
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

(define-public kraken
  (package
    (name "kraken")
    (version "0.10.5-beta")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://ccb.jhu.edu/software/kraken/dl/kraken-"
                    version ".tgz"))
              (sha256
               (base32
                "0srl22kway7h9nv5nimblxzd1indnwvbclbbw4ccxp5cw17cc2kw"))))
    (build-system gnu-build-system)
    (arguments
     `(#:tests? #f ;; no tests
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin")))
               (mkdir-p bin)
               (zero? (system* "./install_kraken.sh" bin))))))))
    (native-inputs
     `(("perl" ,perl)))
    (home-page "http://ccb.jhu.edu/software/kraken/")
    (synopsis "Taxonomic sequence classification system")
    (description "Kraken is a system for assigning taxonomic labels to short DNA
sequences, usually obtained through metagenomic studies.")
    (license license:gpl3+)))

(define-public srst2
  (package
    (name "srst2")
    (version "0.2.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append "https://github.com/katholt/srst2/archive/v"
                           version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "08sis735qq3f9f9d81d3qbbi9c8fk828hjqac6jxyf80y2ymjb1m"))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ;; python-2 only
       #:tests? #f ;; TODO: fix tests
       #:phases
       (modify-phases %standard-phases
         ;; XXX this fails
         ;; (replace 'check
         ;;   (lambda* _
         ;;     (zero? (system* "python" "-m" "unittest" "discover" "tests/"))))
         (add-after 'install 'install-srst2 ;; console script not in install target
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (data (string-append out "/share/srst2/data")))
               (copy-file "scripts/srst2.py" (string-append bin "/srst2"))
               (mkdir-p data)
               (copy-recursively "data" data)))))))
    (native-inputs
     `(("python2-mock" ,python2-mock)))
    (propagated-inputs
     `(("python2-scipy" ,python2-scipy "out")
       ("bowtie" ,bowtie)
       ("samtools" ,samtools)))
    (home-page "https://katholt.github.io/srst2/")
    (synopsis "Short Read Sequence Typing for Bacterial Pathogens")
    (description "This program is designed to take Illumina sequence data,
a MLST database and/or a database of gene sequences (e.g. resistance genes,
virulence genes, etc) and report the presence of STs and/or reference genes.")
    (license license:bsd-3)))

(define-public concoct
  (package
    (name "concoct")
    (version "0.4.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/BinPro/CONCOCT/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (modules '((guix build utils)))
       (sha256
        (base32
         "1c4rsnpfxyrdamf61h22d2bmlca2x9kh1gbzd8rm8vypvm86w3n0"))
       (snippet
        '(substitute* "setup.py"
           ;; remove hard dependency on old nose version
           (("nose==1.3.0") "nose>=1.3.0")))))
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2 ;; requires itertools izip
       #:configure-flags '("--single-version-externally-managed"
                           "--root=/") ;; ^ avoid making egg
       #:tests? #f)) ;; TODO: fix tests
;       #:phases
;       (modify-phases %standard-phases
;         (delete 'check)
;         (add-after 'install 'check ;; it's easier to check after install
;           (lambda* (#:key inputs outputs #:allow-other-keys)
;             ;; make sure concoct is in python path for tests
;             (setenv "PYTHONPATH"
;                     (string-append
;                      (getenv "PYTHONPATH")
;                      ":" (assoc-ref outputs "out")
;                      "/lib/python"
;                      (string-take (string-take-right
;                                    (assoc-ref inputs "python") 5) 3)
;                      "/site-packages"))
;             (zero? (system* "nosetests" "-v" )))))
    (inputs
     `(("gsl" ,gsl)))
    (native-inputs
     `(("python-nose" ,python2-nose)
       ("python-cython" ,python2-cython)
       ("python-docutils" ,python2-docutils)
       ("python-setuptools" ,python2-setuptools)
       ("python-sphinx" ,python2-sphinx)
       ("python-sphinx-rtd-theme" ,python2-sphinx-rtd-theme)))
    (propagated-inputs
     `(("python-scipy" ,python2-scipy)
       ("python-numpy" ,python2-numpy)
       ("python-pandas" ,python2-pandas)
       ("python-biopython" ,python2-biopython)
       ("python-tz" ,python2-pytz) ;; XXX propagate this from python-pandas
       ("python-scikit-learn" ,python2-scikit-learn)))
    (home-page "https://github.com/BinPro/CONCOCT")
    (synopsis "Clustering cONtigs with COverage and ComposiTion")
    (description "Concoct is a program that combines three types of
information - sequence composition, coverage across multiple sample, and
read-pair linkage - to automatically bin metagenomic contigs into genomes.")
    (license license:bsd-2)))

(define-public qiime2
  (package
    (name "qiime2")
    (version "2.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/qiime2/qiime2/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0f0725s9r87r9rqgzfyfyr5yzhdvi5r6xf0yn71s9b6lc5n0wlrw"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-pyyaml" ,python-pyyaml)
       ("python-decorator" ,python-decorator)
       ("python-cookiecutter" ,python-cookiecutter)
       ("python-ipymd" ,python-ipymd)
       ("python-jupyter" ,python-jupyter)
       ("python-frontmatter" ,python-frontmatter)
       ("python-pandas" ,python-pandas)))
    (home-page "http://qiime.org")
    (synopsis "Quantitative Insights Into Microbial Ecology")
    (description "QIIME is a bioinformatics pipeline for performing microbiome analysis
from raw DNA sequencing data.  QIIME is designed to take users from raw sequencing data
generated on the Illumina or other platforms through publication quality graphics and
statistics.  This includes demultiplexing and quality filtering, OTU picking, taxonomic
assignment, and phylogenetic reconstruction, and diversity analyses and visualizations.")
    (license license:bsd-3)))

(define-public q2cli
  (package
    (name "q2cli")
    (version "0.0.3")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/qiime2/q2cli/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1x5ykghj3zpjp3kjd7pmc837xgyq0y4l0h4radl66r431mdif91x"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-click" ,python-click)
       ("qiime2" ,qiime2)))
    (home-page "http://qiime.org")
    (synopsis "Command line interface for QIIME 2")
    (description "This package contains the CLI client for qiime2.")
    (license license:bsd-3)))

(define-public python-scikit-bio
  ;; XXX non-deterministic build
  (package
    (name "python-scikit-bio")
    (version "0.5.0")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/biocore/scikit-bio/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1ddgx31bnwn5936852nxn3a8bag8vqcz645k1jlr5z2kjk2ddhs9"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f)) ;; TODO tests require GTK
    (native-inputs
     `(("python-coverage" ,python-coverage)
       ("python-nose" ,python-nose)
       ("python-requests" ,python-requests)))
    (propagated-inputs
     `(("python-cachecontrol" ,python-cachecontrol)
       ("python-decorator" ,python-decorator)
       ("python-ipython" ,python-ipython)
       ("python-lockfile" ,python-lockfile)
       ("python-natsort" ,python-natsort)
       ("python-numpy" ,python-numpy)
       ("python-pandas" ,python-pandas)
       ("python-scipy" ,python-scipy)))
    (home-page "http://scikit-bio.org/")
    (synopsis "Python toolkit for bioinformatics")
    (description "Data structures, algorithms, and educational resources for
bioinformatics.")
    (license license:bsd-3)))

(define-public q2-types
  (package
    (name "q2-types")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/qiime2/q2-types/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0s8r0291ia51rsmpah6cd1w6w4zw74jv0b1jh3vpf4awrlf7484r"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;; FIXME: tests require GTK
       #:configure-flags '("--single-version-externally-managed"
                          "--root=/"))) ;; avoid making eggs
    ;#:phases
    ;(modify-phases %standard-phases
    ;  (replace 'check
    ;    (lambda _ (zero? (system* "nosetests")))))))
    ;(native-inputs
    ; `(("python-nose" ,python-nose)
    ;   ("python-future" ,python-future)))
    (propagated-inputs
     `(("python-scikit-bio" ,python-scikit-bio)
       ("python-pandas" ,python-pandas)
       ("python-ijson" ,python-ijson)
       ("python-biom-format" ,python-biom-format)
       ("qiime2" ,qiime2)))
    (home-page "http://qiime.org")
    (synopsis "Common QIIME 2 semantic types")
    (description "Definitions of common QIIME 2 types")
    (license license:bsd-3)))

(define-public q2-feature-table
  (package
    (name "q2-feature-table")
    (version "0.0.1")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/qiime2/" name "/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "1f9rp6d8vh4m1s6xfn1ylpsidqwnrkdr8f2wm1brizz5pkxpwp3k"))))
    (build-system python-build-system)
    (arguments
     `(#:tests? #f ;; FIXME: tests require GTK
       #:configure-flags '("--single-version-externally-managed"
                           "--root=/"))) ;; avoid making eggs
       ;#:phases
       ;(modify-phases %standard-phases
       ;  (replace 'check
       ;    (lambda _ (zero? (system* "nosetests")))))))
    ;(native-inputs
    ; `(("python-nose" ,python-nose)
    ;   ("python-future" ,python-future)))
    (propagated-inputs
     `(("python-biom-format" ,python-biom-format)
       ("python-scipy" ,python-scipy)
       ("python-seaborn" ,python-seaborn)
       ("q2-types" ,q2-types)
       ("qiime2" ,qiime2)))
    (home-page "http://qiime.org")
    (synopsis "QIIME2 plugin for working with sample x feature tables")
    (description "Official QIIME 2 plugin supporting basic operations on
sample x feature tables.")
    (license license:bsd-3)))

(define-public q2-diversity
  (package
    (name "q2-diversity")
    (version "0.0.2")
    (source
     (origin
       (method url-fetch)
       (uri (string-append
             "https://github.com/qiime2/" name "/archive/"
             version ".tar.gz"))
       (file-name (string-append name "-" version ".tar.gz"))
       (sha256
        (base32
         "0snalib8yhs9qvbj9qf0g8d4cp21bj5f8ia58wz6rr7xifdfgw9m"))))
    (build-system python-build-system)
    (propagated-inputs
     `(("python-biom-format" ,python-biom-format)
       ("python-numpy" ,python-numpy)
       ("python-scipy" ,python-scipy)
       ("python-scikit-bio" ,python-scikit-bio)
       ("python-pandas" ,python-pandas)
       ("python-seaborn" ,python-seaborn)
       ("python-statsmodels" ,python-statsmodels)
       ("q2-types" ,q2-types)
       ("q2-feature-table" ,q2-feature-table)
       ("qiime2" ,qiime2)))
    (home-page "http://qiime.org")
    (synopsis "QIIME2 plugin for core diversity analyses")
    (description "This plugin provides functionality for performing alpha and
beta diversity analyses (as well as some related statistics and visualizations)
in QIIME 2.")
    (license license:bsd-3)))

(define-public prokka
  (package
    (name "prokka")
    (version "1.11")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.vicbioinformatics.com/prokka-"
                    version ".tar.gz"))
              (sha256
               (base32
                "1bh1h6bk6sjzcsvw8qjkbnfvg9c6yqsvna4ydj7arrlgfrn1867f"))
              (modules '((guix build utils)))
              (snippet
               ;; Drop bundled binaries.
               '(delete-file-recursively "binaries"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f ; No tests.
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (doc (string-append out "/doc"))
                    (db (string-append out "/db")))
               (for-each (λ (f) (install-file f bin))
                         (find-files "bin" "prokka.*"))
               (copy-recursively "doc" doc)
               (copy-recursively "db" db)))))))
    ;; TODO: Replace paths instead of propagating.
    ;; It also bundles NCBI 'tbl2asn' for which no source is available.
    (propagated-inputs
     `(("aragorn" ,aragorn)
       ("blast+" ,blast+)
       ("hmmer" ,hmmer)
       ("infernal" ,infernal)
       ("minced" ,minced)
       ("parallel" ,parallel)
       ("prodigal" ,prodigal)
       ("bioperl-minimal" ,bioperl-minimal)
       ("perl-xml-simple" ,perl-xml-simple)))
    (home-page "https://github.com/tseemann/prokka")
    (synopsis "Rapid prokaryotic genome annotation")
    (description "Prokka is a tool to annotate bacterial, archaeal and
viral genomes quickly and produce standards-compliant output files.")
    (license license:gpl3+)))

(define-public nullarbor
  (package
    (name "nullarbor")
    (version "1.01")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "https://github.com/tseemann/nullarbor/archive/v"
                    version ".tar.gz"))
              (file-name (string-append name "-" version ".tar.gz"))
              (sha256
               (base32
                "0lx4llycamwdmp4rkgzrkww1prl73v8pgr2gnh3plfqb0ialimxk"))))
    (build-system perl-build-system)
    (arguments
     `(#:tests? #f
       #:phases
       (modify-phases %standard-phases
         (delete 'configure)
         (delete 'build)
         (replace 'install
           (lambda* (#:key outputs #:allow-other-keys)
             (let ((out (assoc-ref outputs "out")))
               (copy-recursively "." out)))))))
    ;; TODO: Replace paths instead of propagating.
    (propagated-inputs
     `(("automake" ,automake)
       ("ghc-pandoc" ,ghc-pandoc)
       ("imagemagick" ,imagemagick)
       ("kraken" ,kraken)
       ("prokka" ,prokka)
       ("roary" ,roary)
       ("trimmomatic" ,trimmomatic)
       ("bioperl-minimal" ,bioperl-minimal)
       ("perl-moo" ,perl-moo)
       ("perl-yaml-tiny" ,perl-yaml-tiny)))
    (home-page "https://github.com/tseemann/nullarbor")
    (synopsis "Pipeline to generate microbiology reports from sequenced isolates")
    (description "Nullarbor is a pipeline to generate complete public health
microbiology reports from sequenced isolates.")
    (license license:gpl2)))

(define-public trimmomatic
  (package
    (name "trimmomatic")
    (version "0.36")
    (source (origin
              (method url-fetch)
              (uri (string-append
                    "http://www.usadellab.org/cms/uploads/supplementary/"
                    "Trimmomatic/Trimmomatic-Src-" version ".zip"))
              (sha256
               (base32
                "08dr28248vpcjqx52zz1vg3s50x5bfqlh1n5z2816a4r0waimfyc"))))
    ;; XXX: This package bundles jbzip2.jar, for which no official source exists.
    (build-system ant-build-system)
    (arguments
     `(#:tests? #f
       #:build-target "dist"
       #:phases
       (modify-phases %standard-phases
         (replace 'install ; No install target.
           (lambda* (#:key inputs outputs #:allow-other-keys)
             (let* ((out (assoc-ref outputs "out"))
                    (bin (string-append out "/bin"))
                    (jar-name (string-append "trimmomatic-" ,version ".jar"))
                    (wrapper (string-append bin "/trimmomatic")))
           (install-file (string-append "dist/jar/" jar-name) bin)
           (with-output-to-file wrapper
             (lambda _
               (display
                (string-append
                 "#!" (assoc-ref inputs "bash") "/bin/sh\n\n"
                 (assoc-ref inputs "jre") "/bin/java -jar "
                 bin "/" jar-name " \"$@\"\n"))))
           (chmod wrapper #o555)))))))
    (native-inputs
     `(("bash" ,bash)
       ("jdk" ,icedtea "jdk")
       ("unzip" ,unzip)))
    (inputs
     `(("jre" ,icedtea "out")))
    (home-page "http://www.usadellab.org/cms/index.php?page=trimmomatic")
    (synopsis "Flexible read trimming tool for Illumina NGS data")
    (description
     "Trimmomatic performs a variety of useful trimming tasks for illumina
paired-end and single ended data.The selection of trimming steps and their
associated parameters are supplied on the command line.")
    (license (list license:gpl3 license:expat)))) ; jbzip2 is MIT

(define-public python2-pbcommand
  ;; Upstream does not tag git releases and PyPi is out of date.
  ;; See https://github.com/PacificBiosciences/pbcommand/issues/116
  (let ((revision "1")
        (commit "e8d577ac3f92a2dedae1470fcf00cabdf22ce824"))
    (package
      (name "python2-pbcommand")
      (version (string-append "0.4.11-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PacificBiosciences/pbcommand.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "0vflh06q23swbdps22rv0qwbsj550mw94ifn5zc84pps8irnkiiq"))))
      (build-system python-build-system)
      (arguments
       `(#:python ,python-2
         #:configure-flags '("--single-version-externally-managed" "--root=/")
         #:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'substitute-/bin/bash
             (lambda _
               ;; Fully qualify /bin/bash for running external commands.
               (substitute* "pbcommand/engine/runner.py"
                 (("/bin/bash") (which "bash")))))
           (replace 'check
             (lambda _ (zero? (system* "nosetests" "-v")))))))
      (native-inputs
       `(("python2-nose" ,python2-nose)
         ("python2-sphinx-bootstrap-theme" ,python2-sphinx-bootstrap-theme)
         ("python2-sphinx-argparse" ,python2-sphinx-argparse)
         ("python2-tox" ,python2-tox)
         ("python2-setuptools" ,python2-setuptools)))
      (propagated-inputs
       `(("python2-functools32" ,python2-functools32)
         ("python2-jsonschema" ,python2-jsonschema)
         ("python2-numpy" ,python2-numpy "out")
         ("python2-avro" ,python2-avro)
         ("python2-requests" ,python2-requests)
         ("python2-iso8601" ,python2-iso8601)))
      (home-page "https://github.com/PacificBiosciences/pbcommand")
      (synopsis "PacBio common models and CLI tool contract interface")
      (description "PacBio library for common utils, models, and tools
to interface with pbsmrtpipe workflow engine.")
      (license license:bsd-3))))

(define-public consensuscore
  ;; Upstream uses another revision control system and does not make
  ;; git tags for all versions. So we use raw commits instead.
  (let ((revision "1")
        (commit "b579c738f24061f430914737f10cccb9212cc9cf"))
    (package
      (name "consensuscore")
      (version (string-append "1.0.2-" revision "." (string-take commit 7)))
      (source (origin
                (method git-fetch)
                (uri (git-reference
                      (url "https://github.com/PacificBiosciences/ConsensusCore.git")
                      (commit commit)))
                (file-name (string-append name "-" version "-checkout"))
                (sha256
                 (base32
                  "1jpzgnk8grk6amxd5drcgabzr3ymd7ib1cqhw65c6s1y1m1qd14y"))))
      (build-system gnu-build-system)
      (arguments
       `(#:phases
         (modify-phases %standard-phases
           (add-after 'unpack 'patch-makefile
             (lambda _
               (substitute* "make/Defs.mk"
                 (("SHELL=/bin/bash")
                  (string-append "SHELL=" (which "bash"))))
               #t))
           (replace 'configure
             (lambda* (#:key inputs #:allow-other-keys)
               (zero? (system*
                       "./configure"
                       (string-append "--boost="
                                      (assoc-ref inputs "boost") "/include")
                       (string-append "--swig="
                                      (assoc-ref inputs "swig") "/bin/swig")))))
           (replace 'install ; No install target.
             (lambda* (#:key outputs #:allow-other-keys)
               (let* ((out (assoc-ref outputs "out"))
                      (lib (string-append out "/lib"))
                      (include (string-append out "/include")))
                 (install-file "build/C++/libConsensusCore.a" lib)
                 (copy-recursively "include" include)))))))
      (native-inputs
       `(("python", python-2)
         ("swig" ,swig)
         ;("googletest" ,googletest) ; TODO: remove bundled gtest.
         ("python-wrapper" ,python-wrapper)))
      (inputs
       `(("boost" ,boost)))
      (home-page "https://github.com/PacificBiosciences/ConsensusCore")
      (synopsis
       "C++ library of multiple-sequence consensus routines with SWIG bindings")
      (description
       "ConsensusCore is a library of C++ algorithms for PacBio multiple sequence
consensus that powers Quiver (Python) and ConsensusTools (.NET).")
      (license license:bsd-3))))

(define-public python2-consensuscore
  (package
    (inherit consensuscore) ; For version, source and description.
    (name "python2-consensuscore")
    (build-system python-build-system)
    (arguments
     `(#:python ,python-2
       #:tests? #f ; TODO: there is a 'test-python' make target but it needs setup.
       #:phases
       (modify-phases %standard-phases
         (add-after 'unpack 'patch-makefiles
           (lambda _
             (substitute* "make/Defs.mk"
               (("SHELL=/bin/bash") (string-append
                                     "SHELL=" (which "bash"))))
             ;; Prevent library from being built twice.
             (substitute* "Makefile"
               (("^python: lib") "python:"))
             (substitute* "make/Python.mk"
               (("^\\$\\(PYTHON_DLL\\): \\$\\(SWIG_INTERFACES\\) \\$\\(CXX_LIB\\)")
                "$(PYTHON_DLL): $(SWIG_INTERFACES)"))
             #t))
         (add-before 'build 'configure
           (lambda* (#:key inputs #:allow-other-keys)
             (zero?
              (system*
               "./configure"
               (string-append "--boost=" (assoc-ref inputs "boost") "/include")
               (string-append "--swig=" (assoc-ref inputs "swig") "/bin/swig")
               (string-append "--python-include=" (assoc-ref inputs "python")
                              "/include/python2.7")
               (string-append "--numpy-include=" (assoc-ref inputs "python2-numpy")
                              "/lib/python2.7/site-packages/numpy"
                              "/core/include")))))
         (replace 'build
           (lambda* (#:key inputs #:allow-other-keys)
             (zero? (system* "make" "python"
                             (string-append "CXX_LIB="
                                            (assoc-ref inputs "consensuscore")
                                            "/lib/libConsensusCore.a")))))
         (add-before 'install 'patch-setup.py
           (lambda _
             (substitute* "setup.py"
               ;; setup.py contains a hack that adds "build" to
               ;; the install command. We need to un-do that.
               (("sys\\.argv\\.insert\\(installPos, \"build\"\\)")
                "True"))
             #t)))))
    (native-inputs
     `(("python2-setuptools" ,python2-setuptools)
       ("consensuscore" ,consensuscore)
       ("python2-numpy" ,python2-numpy "out")
       ("swig" ,swig)))
    (inputs
     `(("boost" ,boost)))
    (synopsis "Python interface to the ConsensusCore algorithms")))
