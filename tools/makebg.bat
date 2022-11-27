@ECHO OFF
:: Version 1.01, feb. 2016
:: Changes:
:: 1.01 - makebg makes a header with all the extra info
::      - include labels for tiles, tlemap, etc., for quick access in code
::      - fixed column calculator bug
:: 1.00 - create for my first GG experiments
::
:: Make background image data file.
:: makebg - convert an image to an SMS-friendly include file containing data
:: for tiles, tilemap and colors (Game Gear format).
:: Maxim's BMP2Tile does all the hard work.
::
:: Drop a properly formatted image on this batch file.
::
:: makebg requires ImageMagick installed and added to the windows PATH.
::
:: Please note! makebg requires the correct path to BMP2Tile.exe, see below
:: and adjust as necessary:
:: ****************************************************************************
   set BMP2TilePath=C:\Users\AndersSkriver\Documents\SMSDev\BMP2Tile\bmp2tile.exe
:: ****************************************************************************

::   Abort on incorrect command line argument
IF /I "%~1"=="" GOTO :errormsg
IF NOT EXIST %~1 GOTO :errormsg

:: Use ImageMagick to convert the image to 16 colors.
convert -colors 16 %1 %1

:: Make BMP2Tile process the image.
ECHO Processing image...
%BMP2TilePath% %1 -palgg -savetiles %1_tiles.inc -savetilemap %1_tilemap.inc -savepalette %1_palette.inc -exit

:: Calculate amount of tiles (infer from amount of lines in the file).
SETLOCAL ENABLEDELAYEDEXPANSION
SET "cmd=findstr /R /N "^^" %1_tiles.inc | find /C ":""
FOR /f %%a IN ('!cmd!') DO SET tiles=%%a
SET /a tiles=%tiles%/2
IF %tiles% LEQ 256 GOTO TilesOK
ECHO *Error* The image contains %tiles% tiles. It can contain a maximum of 256 tiles. Please reformat the image and try again.
GOTO :eof
:TilesOK

:: Calculate the amount of colors (based on the length of the file).
SET /p palette=<%1_palette.inc
CALL :strlen colors palette
:: Subtract 3 (the ".dw " part) and divide by 6 to get amount of colors in the image.
SET /a colors=(%colors%-3)/6
:: Add colors up to 16 if necessary.
:: IF %colors%==16 GOTO Skip
:: ECHO ; >> %1_palette.inc
:: FOR /l %%i IN (%colors%,1,15) DO @ECHO .dw $0000 >> %1_palette.inc
:: Skip

:: Calculate tilemap dimensions (based on the file).
:: Start by inferring number of columns from the length of the first string.
:: SET /p tilemap=<%1_tilemap.inc
:: CALL :strlen columns tilemap
:: Subtract 4 (the ".dw" part) and divide by 6 to get columns (width).
:: SET /a columns=(%columns%-4)/6

:: Skip columns check
:: IF %columns%==32 GOTO ColumnsOK
:: ECHO Image does not conform to the required width (256 pixels/32 tile columns).
:: GOTO :eof

:::ColumnsOK
:: Then calculate the number of rows (infer from no. of lines in the file).
:: SET "cmd=findstr /R /N "^^" %1_tilemap.inc | find /C ":""
:: FOR /f %%a IN ('!cmd!') DO SET rows=%%a
:: Skip row check.
:: IF %rows%==24 GOTO RowsOK
:: ECHO Image does not conform to the required height (192 pixels/24 rows).
:: GOTO :eof

:::RowsOK
:: Merge all the files into one file
SET filename=%~n1
SET char=%filename:~0,1%
CALL :UpCase char
SET label=%filename:~1%
SET label=%char%%label%


GOTO SkipCollectedFile

:: Create collected file
SET incfile=%filename%.inc
ECHO .section "%label%" free > %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO ; Made with makebg %date% %time% >> %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO %label%: >> %incfile%
ECHO ; Image info: >> %incfile%
ECHO .db %tiles% ; Number of tiles >> %incfile%
ECHO .db %colors% ; Number of colors >> %incfile%
ECHO. >> %incfile%
ECHO %label%_tilemap: >> %incfile%
TYPE %1_tilemap.inc >> %incfile%
ECHO. >> %incfile%
ECHO %label%_palette: >> %incfile%
TYPE %1_palette.inc >> %incfile%
ECHO. >> %incfile%
ECHO. >> %incfile%
ECHO %label%_tiles: >> %incfile%
TYPE %1_tiles.inc >> %incfile%
ECHO .ends >> %incfile%

:SkipCollectedFile

:: Create tile file
SET incfile=%filename%Tiles.inc
ECHO .section "%label%Tiles" free > %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO ; Made with makebg %date% %time% >> %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO %label%TileNum: >> %incfile%
ECHO  .db %tiles% ; Number of tiles >> %incfile%
ECHO %label%Tiles: >> %incfile%
TYPE %1_tiles.inc >> %incfile%
ECHO. >> %incfile%
ECHO %label%TilesEnd: >> %incfile%
ECHO .ends >> %incfile%

:: Create palette file
SET incfile=%filename%Palette.inc
ECHO .section "%label%Palette" free > %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO ; Made with makebg %date% %time% >> %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO %label%PaletteSize: >> %incfile%
ECHO  .db %colors% ; Number of colors >> %incfile%
ECHO %label%Palette: >> %incfile%
TYPE %1_palette.inc >> %incfile%
ECHO. >> %incfile%
ECHO %label%PaletteEnd: >> %incfile%
ECHO .ends >> %incfile%

:: Create tilemap file
SET incfile=%filename%TileMap.inc
ECHO .section "%label%TileMap" free > %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO ; Made with makebg %date% %time% >> %incfile%
ECHO ; ============================================================================= >> %incfile%
ECHO. >> %incfile%
ECHO %label%TileMap: >> %incfile%
TYPE %1_tilemap.inc >> %incfile%
ECHO .ends >> %incfile%



:: Clean up
DEL %1_tiles.inc
DEL %1_tilemap.inc
DEL %1_palette.inc


ECHO Conversion profile of image: %tiles% tiles, %columns% columns, and %rows% rows.
ECHO The include file %incfile% was created. Operation complete...
GOTO :eof

:: This function returns the length of a string.
:strlen <resultVar> <stringVar>
(
    SETLOCAL ENABLEDELAYEDEXPANSION
    SET "s=!%~2!#"
    SET "len=0"
    FOR %%P IN (4096 2048 1024 512 256 128 64 32 16 8 4 2 1) do (
        IF "!s:~%%P,1!" NEQ "" (
            SET /a "len+=%%P"
            SET "s=!s:~%%P!"
        )
    )
)
(
    ENDLOCAL
    SET "%~1=%len%"
    EXIT /b
)

:toUpper
:UpCase
:: Subroutine to convert a variable VALUE to all upper case.
:: The argument for this subroutine is the variable NAME.
SET %~1=!%1:a=A!
SET %~1=!%1:b=B!
SET %~1=!%1:c=C!
SET %~1=!%1:d=D!
SET %~1=!%1:e=E!
SET %~1=!%1:f=F!
SET %~1=!%1:g=G!
SET %~1=!%1:h=H!
SET %~1=!%1:i=I!
SET %~1=!%1:j=J!
SET %~1=!%1:k=K!
SET %~1=!%1:l=L!
SET %~1=!%1:m=M!
SET %~1=!%1:n=N!
SET %~1=!%1:o=O!
SET %~1=!%1:p=P!
SET %~1=!%1:q=Q!
SET %~1=!%1:r=R!
SET %~1=!%1:s=S!
SET %~1=!%1:t=T!
SET %~1=!%1:u=U!
SET %~1=!%1:v=V!
SET %~1=!%1:w=W!
SET %~1=!%1:x=X!
SET %~1=!%1:y=Y!
SET %~1=!%1:z=Z!
EXIT /b


:errormsg
:: This is the error message.
ECHO *Error* Correct usage: makebg [source image]
ECHO Drag and drop an image on makebg.bat to process it.
