# Copyright (C) Tal Galili
#
# This file is part of installr.
#
# installr is free software: you can redistribute it and/or modify it
# under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 2 of the License, or
# (at your option) any later version.
#
# installr is distributed in the hope that it will be useful, but
# WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
#  A copy of the GNU General Public License is available at
#  http://www.r-project.org/Licenses/
#




#' @title Converts xls(x) to csv using VB
#' @export
#' @description 
#' Converts xls(x) to csv using VB script. Not that important now that we have the readxl package.
#' @param xlsx the (character) name of the xlsx (or xls) file to convert.
#' if xlsx has a full path, it will override the path parameter.
#' @param csv the (character) name of the csv file to convert to (default will be the name of the xlsx file)
#' @param path the path for the files (default is the working diractory).
#' @param ... ignored.
#' @return NULL
#' @source
#' This is based on the code from plang's answer here:
#' \url{http://stackoverflow.com/questions/1858195/convert-xls-to-csv-on-command-line}
#' @examples
#' \dontrun{
#' 
#' xlsx2csv("c:/some_file.xlsx")
#' 
#' }
xlsx2csv <- function(xlsx, csv, path, ...) {
#    xlsx <- "a.xlsx"
#    ?gsub
   if(!grepl("\\.xlsx || \\.xls", xlsx)) warning("File is missing .xlsx in its name - is it valid?")
   
#    txt <- "c:\\aaa.xlsx" ; grepl("\\\\", txt) | grepl("\\/", txt)
#    txt <- "c:/aaa.xlsx" ; grepl("\\\\", txt) | grepl("\\/", txt)
#    txt <- "aaa.xlsx" ; grepl("\\\\", txt) | grepl("\\/", txt)
   
   if(grepl("\\\\", xlsx) | grepl("\\/", xlsx)) {
      # then we have a path in xlsx!

#       xlsx <- "c:\\a/b\\c.xlsx"
      xlsx <- gsub("\\\\", "\\/", xlsx) # unify path
      splitted_xlsx <- strsplit(xlsx, "\\/")[[1]]
      path <- paste(head(splitted_xlsx, -1), collapse = "/")
      xlsx <- tail(splitted_xlsx, 1)
      
   } else {
      if(missing(path)) path <- getwd()
   }
   
   
   
   if(missing(csv)) {
      csv <- gsub("\\.xlsx", "", xlsx)
      csv <- gsub("\\.xls", "", xlsx)      
      csv <- paste0(csv, ".csv")      
   }
   
   VB_code <-' 
      if WScript.Arguments.Count < 2 Then
          WScript.Echo "Please specify the source and the destination files. Usage: ExcelToCsv <xls/xlsx source file> <csv destination file>"
          Wscript.Quit
      End If
      
      csv_format = 6
      
      Set objFSO = CreateObject("Scripting.FileSystemObject")
      
      src_file = objFSO.GetAbsolutePathName(Wscript.Arguments.Item(0))
      dest_file = objFSO.GetAbsolutePathName(WScript.Arguments.Item(1))
      
      Dim oExcel
      Set oExcel = CreateObject("Excel.Application")
      
      Dim oBook
      Set oBook = oExcel.Workbooks.Open(src_file)
      
      oBook.SaveAs dest_file, csv_format
      
      oBook.Close False
      oExcel.Quit'
   
#    path <- "c:\\"
   
   xlsx2csv.vbs <- file.path(path, "xlsx2csv.vbs")
   writeLines(VB_code, xlsx2csv.vbs)
   txt2run <- paste0(xlsx2csv.vbs, " ", 
                     file.path(path, xlsx), " ",
                     file.path(path, csv))
   
   success <- shell(txt2run)   
   unlink(xlsx2csv.vbs)
   invisible(success)
}


