(define (filename-basename orig-name)
  (let* ((name-without-path (car (last (strbreakup orig-name "\\"))))
         (name-without-extension (car (strbreakup name-without-path "."))))
    name-without-extension))

(define (batch-logo-remover input-folder output-folder logo-x logo-y logo-width logo-height)
  (let* ((filelist (cadr (file-glob (string-append input-folder "\\*.png") 1))))
    (while (not (null? filelist))
      (let* ((filename (car filelist))
             (image (car (gimp-file-load RUN-NONINTERACTIVE filename filename)))
             (drawable (car (gimp-image-get-active-layer image))))
        (gimp-rect-select image logo-x logo-y logo-width logo-height CHANNEL-OP-REPLACE FALSE 0)
        (python-fu-heal-selection RUN-NONINTERACTIVE image drawable 50 TRUE TRUE)
        (gimp-selection-none image)
        (let ((output-filename (string-append output-folder "\\" (filename-basename filename) ".png")))
          (gimp-file-save RUN-NONINTERACTIVE image drawable output-filename output-filename)
          (gimp-image-delete image)))
      (set! filelist (cdr filelist)))))

(script-fu-register "batch-logo-remover"
                    "<Toolbox>/MyScripts/Batch Logo Remover"
                    "Batch remove logos from a folder of PNGs using Resynthesizer's heal-selection plugin"
                    "Rener Almeida"
                    "2023, Rener Almeida" 
                    "2023"
                    ""
                    SF-DIRNAME "Input folder" ""
                    SF-DIRNAME "Output folder" ""
                    SF-ADJUSTMENT "Logo X" '(0 0 10000 1 10 0 1)
                    SF-ADJUSTMENT "Logo Y" '(0 0 10000 1 10 0 1)
                    SF-ADJUSTMENT "Logo Width" '(100 1 10000 1 10 0 1)
                    SF-ADJUSTMENT "Logo Height" '(100 1 10000 1 10 0 1))

