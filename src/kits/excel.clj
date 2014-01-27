(ns kits.excel
  (:require [kits.homeless :refer [defn-kw]])
  (:import (java.io FileInputStream FileOutputStream)
           (java.text DecimalFormat)
           (org.apache.poi.hssf.usermodel HSSFClientAnchor
                                          HSSFRichTextString
                                          HSSFWorkbook)
           (org.apache.poi.ss.usermodel Cell CellStyle CreationHelper
                                        Sheet Workbook WorkbookFactory)
           (org.apache.poi.ss.util CellRangeAddress)))


(defn save-workbook [wb, file-name]
  (.write wb (FileOutputStream. file-name)))

(defn open-file [file-name]
  (WorkbookFactory/create
   (FileInputStream. file-name)))

(defn new-excel-workbook []
  (HSSFWorkbook.))

(defn row-at [sheet row-num]
  (let [row (.getRow sheet row-num)]
    (if row
      row
      (.createRow sheet row-num))))

(defn cell-at [sheet i j]
  (let [cell (.getCell (row-at sheet (dec i)) (dec j))]
    (if (nil? cell) (.createCell (row-at sheet (- i 1)) (- j 1)) cell))) ; subtract 1 to make it match with excels row nums, easier to match

(defn get-sheet [wb sheet-name]
  (.getSheet wb sheet-name))

(defn create-sheet [wb sheet-name]
  (.createSheet wb sheet-name))


(defn convert-if-number
  "Attempt to convert `x` to double and return that on success. If
  conversion fails return `x` as it is (probably a string)."
  [x]
  (try
    (double (bigdec
             (.format (DecimalFormat. "#.00")
                      (bigdec x))))
    (catch Exception _
      x)))


(defn set-cell-style-2-decimal-places
  "Set the cell style as right-aligned number with 2 decimal places."
  [^Cell cell]
  (let [^Workbook       wb (.getWorkbook ^Sheet (.getSheet cell))
        ^CreationHelper ch (.getCreationHelper wb)
        ^CellStyle      cs (.getCellStyle cell)]
    (doto cs
      (.setDataFormat (-> ch
                          .createDataFormat
                          (.getFormat "0.00")))
      (.setAlignment CellStyle/ALIGN_RIGHT))
    (.setCellStyle cell cs)))


(defn-kw set-cell-value [^Sheet sheet row column amount & {:keys [decimal-places?]}]
  "Set cell value with appropriate formatting."
  (let [cell (cell-at sheet row column)
        amt  (convert-if-number amount)]
    (.setCellValue cell amt)
    (when (and (number? amt)
               decimal-places?)
      (set-cell-style-2-decimal-places cell))))

(defn set-data-from-positions
  ([positions sheet data]
     (doseq [[key [row column] format-type] positions]
       (set-cell-value sheet row column (format-type (data key))))))

(defn merge-cells [sheet start-row end-row start-col end-col]
  (.addMergedRegion sheet (CellRangeAddress. start-row end-row start-col end-col)))

(defn remove-sheet [wb sheet-name]
  (.removeSheetAt wb (.getSheetIndex wb sheet-name)))

(defn create-textbox [sheet]
  (let [patriarch (.createDrawingPatriarch sheet)
        client-anchor (HSSFClientAnchor. 0 0 0 0 2 33 3 35)
        textbox (.createTextbox patriarch client-anchor)]
    (.setMarginTop textbox 10)
    (.setMarginRight textbox 1)
    (.setMarginBottom textbox 2)
    (.setMarginLeft textbox 0.1)
    textbox))

(defn set-font-to-string [wb s]
  (let [font (.createFont wb)]
    (.setBoldweight font 700)
    (.setFontHeightInPoints font 13)
    (.setColor font 0)
    (.applyFont s font)))

(defn set-textbox-string [wb textbox s]
  (let [excel-string (HSSFRichTextString. s)]
    (set-font-to-string wb excel-string)
    (.setString textbox excel-string)))
