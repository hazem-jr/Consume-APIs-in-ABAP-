*&---------------------------------------------------------------------*
*& Report YSAGO_SALES_REPORTS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT YSAGO_SALES_REPORTS.

TABLES : VBRK , KNA1 , ZSD_CUST_QTY_WK .
CLASS LCL_MAIN DEFINITION CREATE PRIVATE .

  PUBLIC SECTION .

    TYPES : BEGIN OF ZSTR1 ,
              SIGN   TYPE TVARV_SIGN,
              OPTION TYPE TVARV_OPTI,
              LOW    TYPE KNA1-KUNNR,
              HIGH   TYPE KNA1-KUNNR,
            END OF ZSTR1 .

    DATA : LT_RANGE TYPE RANGE OF KNA1-KUNNR .

    CLASS-METHODS :

      START IMPORTING IP_CUST_LOW  TYPE  VBRK-KUNAG
                      IP_CUST_HIGH TYPE  VBRK-KUNAG
                      IP_DATE_LOW  TYPE  FKDAT
                      IP_DATE_HIGH TYPE  FKDAT
                      IP_SELECTED  TYPE  CHAR1
                      IP_RANGE     LIKE LT_RANGE ,

      GET_API_TOKEN .

  PROTECTED SECTION .

  PRIVATE SECTION .
    CLASS-METHODS :
      FETCH_DATA_DETAILS  IMPORTING IP_CUST_LOW  TYPE  VBRK-KUNAG
                                    IP_CUST_HIGH TYPE  VBRK-KUNAG
                                    IP_DATE_LOW  TYPE  FKDAT
                                    IP_DATE_HIGH TYPE  FKDAT ,

      FETCH_DATA_TOTALS  IMPORTING IP_CUST_LOW  TYPE  VBRK-KUNAG
                                   IP_CUST_HIGH TYPE  VBRK-KUNAG
                                   IP_DATE_LOW  TYPE  FKDAT
                                   IP_DATE_HIGH TYPE  FKDAT ,
      DISPLAY ,

      SET_PF_STATUS
        CHANGING
          CO_ALV TYPE REF TO CL_SALV_TABLE  .

    TYPES : BEGIN OF TY_SALES ,
              GJAHR TYPE VBRK-GJAHR,
              MONTH TYPE MONTH,
              VKORG TYPE VBRK-VKORG,
              KUNAG TYPE VBRK-KUNAG,
              VBELN TYPE VBRK-VBELN,
              FKDAT TYPE VBRK-FKDAT,
              MATNR TYPE VBRP-MATNR,
              FKIMG TYPE VBRP-FKIMG,
            END OF TY_SALES .

    TYPES : BEGIN OF TY_TOTALS ,
              GJAHR TYPE VBRK-GJAHR,
              MONTH TYPE MONTH,
              VKORG TYPE VBRK-VKORG,
              KUNAG TYPE VBRK-KUNAG,
              FKIMG TYPE VBRP-FKIMG,
              NETWR TYPE VBRP-NETWR,
            END OF TY_TOTALS .

    CLASS-DATA : LT_SALES TYPE TABLE OF TY_SALES .
    CLASS-DATA : LT_TOTALS TYPE TABLE OF TY_TOTALS.

ENDCLASS .

CLASS LCL_MAIN IMPLEMENTATION .
  METHOD START .

    IF IP_SELECTED = '1' .
      FETCH_DATA_DETAILS(
        EXPORTING
          IP_CUST_LOW  = IP_CUST_LOW
          IP_CUST_HIGH = IP_CUST_HIGH
          IP_DATE_LOW  = IP_DATE_LOW
          IP_DATE_HIGH = IP_DATE_HIGH
      ).

    ELSE .
      FETCH_DATA_TOTALS(
        EXPORTING
          IP_CUST_LOW  = IP_CUST_LOW
          IP_CUST_HIGH = IP_CUST_HIGH
          IP_DATE_LOW  = IP_DATE_LOW
          IP_DATE_HIGH = IP_DATE_HIGH
      ).
    ENDIF .


  ENDMETHOD .
  """ Sales data details (customer / material / )
  METHOD FETCH_DATA_DETAILS .

    SELECT GJAHR HEADER~VKORG KUNAG
           HEADER~VBELN FKDAT ITEMS~MATNR SUM( ITEMS~FKIMG ) AS FKIMG
      FROM VBRK AS HEADER INNER JOIN VBRP AS ITEMS ON HEADER~VBELN  = ITEMS~VBELN
      INTO CORRESPONDING FIELDS OF TABLE LT_SALES
      WHERE  HEADER~KUNAG BETWEEN IP_CUST_LOW AND IP_CUST_HIGH  AND HEADER~FKDAT BETWEEN IP_DATE_LOW AND IP_DATE_HIGH
      AND MATNR IN ( 'Material codes' ) GROUP BY KUNAG HEADER~VBELN MATNR GJAHR HEADER~VKORG FKDAT.

    DISPLAY( ).

  ENDMETHOD.

  """" total sales data
  METHOD FETCH_DATA_TOTALS .

    SELECT GJAHR HEADER~VKORG KUNAG
            SUM( ITEMS~FKIMG ) AS FKIMG SUM( ITEMS~NETWR ) AS NETWR
      FROM VBRK AS HEADER INNER JOIN VBRP AS ITEMS ON HEADER~VBELN  = ITEMS~VBELN
      INTO CORRESPONDING FIELDS OF TABLE LT_TOTALS
      WHERE  HEADER~KUNAG BETWEEN IP_CUST_LOW AND IP_CUST_HIGH  AND HEADER~FKDAT BETWEEN IP_DATE_LOW AND IP_DATE_HIGH
      AND MATNR IN ( 'Material Codes' ) GROUP BY KUNAG GJAHR HEADER~VKORG .

    DISPLAY( ).

  ENDMETHOD.

  METHOD DISPLAY .

    DATA: GO_ALV TYPE REF TO CL_SALV_TABLE.

    CL_SALV_TABLE=>FACTORY(
      IMPORTING
        R_SALV_TABLE   = GO_ALV
      CHANGING
        T_TABLE        = LT_SALES
    ).

    SET_PF_STATUS(
      CHANGING
        CO_ALV = GO_ALV
    ).
    GO_ALV->DISPLAY( ).

  ENDMETHOD .

  METHOD SET_PF_STATUS.
    CO_ALV->SET_SCREEN_STATUS(
      EXPORTING
        REPORT        = 'YSAGO_SALES_REPORTS'                 " ABAP Program: Current Master Program
        PFSTATUS      = 'PF_STATUS'
        SET_FUNCTIONS = CO_ALV->C_FUNCTIONS_ALL
    ).

  ENDMETHOD .
  METHOD GET_API_TOKEN.

    DATA : LV_SERVICE TYPE STRING,
           LV_RESULT  TYPE STRING.

    LV_SERVICE = 'https://......' .

    BREAK XABAP3 .
    " create client
    CALL METHOD CL_HTTP_CLIENT=>CREATE_BY_URL
      EXPORTING
        URL                = LV_SERVICE
      IMPORTING
        CLIENT             = DATA(LO_HTTP_CLIENT)
      EXCEPTIONS
        ARGUMENT_NOT_FOUND = 1
        PLUGIN_NOT_ACTIVE  = 2
        INTERNAL_ERROR     = 3
        OTHERS             = 4.

    " prepare the request
    DATA(LV_DATA) = CONV STRING('{"userName":"" , "password":""}') .


*    CALL METHOD lo_http_client->request->set_header_field( name  = '~request_method' value = 'POST' ) .
    CALL METHOD LO_HTTP_CLIENT->REQUEST->SET_HEADER_FIELD( NAME  = 'content-type:' VALUE = 'application/json' ) .
    CALL METHOD LO_HTTP_CLIENT->REQUEST->SET_METHOD( 'POST' ) .


*    lo_http_client->request->set_method( if_http_request=>co_request_method_post ).
    lo_http_client->request->set_form_field( name = 'meth' value = 'GEN_PREVIEW' ).
    LO_HTTP_CLIENT->REQUEST->SET_FORM_FIELD( NAME = 'data' VALUE = LV_DATA ).

    " Send request
    LO_HTTP_CLIENT->SEND(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4 ).

    " Catch result
    LO_HTTP_CLIENT->RECEIVE(
      EXCEPTIONS
        HTTP_COMMUNICATION_FAILURE = 1
        HTTP_INVALID_STATE         = 2
        HTTP_PROCESSING_FAILED     = 3
        OTHERS                     = 4 ).

    IF SY-SUBRC NE 0 .
      CALL METHOD LO_HTTP_CLIENT->GET_LAST_ERROR
        IMPORTING
          CODE    = DATA(LV_SUBRC)
          MESSAGE = DATA(LV_ERRORTEXT).

    ENDIF .
    " Catch data
    DATA(LV_STREAM) = LO_HTTP_CLIENT->RESPONSE->GET_CDATA( ).

    " close the client
    LO_HTTP_CLIENT->CLOSE( ).

  ENDMETHOD .

ENDCLASS .

SELECTION-SCREEN BEGIN OF BLOCK B WITH FRAME TITLE TEXT-001.

SELECT-OPTIONS :
S_CUST     FOR   KNA1-KUNNR  ,
S_DATE     FOR   VBRK-FKDAT .

PARAMETERS : P_R1 RADIOBUTTON GROUP RG,   " sales Data
             P_R2 RADIOBUTTON GROUP RG.   " totals

SELECTION-SCREEN END OF BLOCK B .

START-OF-SELECTION .
  DATA LV_SELECTED(1) .

  IF P_R1 = 'X' .
    LV_SELECTED = '1' .
  ELSE .
    LV_SELECTED = '2' .
  ENDIF .

  LCL_MAIN=>START(
    EXPORTING
      IP_CUST_LOW      = S_CUST-LOW
      IP_CUST_HIGH     = S_CUST-HIGH
      IP_DATE_LOW      = S_DATE-LOW
      IP_DATE_HIGH     = S_DATE-HIGH
      IP_SELECTED      = LV_SELECTED

  )...

  LCL_MAIN=>GET_API_TOKEN(  ) .
