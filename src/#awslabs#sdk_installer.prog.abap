"~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-
*  Copyright Amazon.com, Inc. or its affiliates. All Rights Reserved.
*
*  Licensed under the Apache License, Version 2.0 (the "License").
*  You may not use this file except in compliance with the License.
*  You may obtain a copy of the License at
*
*      http://www.apache.org/licenses/LICENSE-2.0
*
*  Unless required by applicable law or agreed to in writing, software
*  distributed under the License is distributed on an "AS IS" BASIS,
*  WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
*  See the License for the specific language governing permissions and
*  limitations under the License.
"-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-
*
*  Usage:   Using transaction SE80 or SE38, copy & paste the contents of
*           this file into a new report in a package of your choice.
*           Activate the report and hit execute.
*
*  Authors: @meyro, @frij
*
"-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~-~--~-~-~-~-

REPORT /awslabs/sdk_installer.

INTERFACE lif_sdk_constants DEFERRED.
INTERFACE lif_sdk_internet_manager DEFERRED.
INTERFACE lif_sdk_report_update_manager DEFERRED.
INTERFACE lif_sdk_certificate_manager DEFERRED.
INTERFACE lif_sdk_file_manager DEFERRED.
INTERFACE lif_sdk_transport_manager DEFERRED.
INTERFACE lif_sdk_job_manager DEFERRED.

INTERFACE lif_ui_constants DEFERRED.

CLASS lcl_sdk_zipfile DEFINITION DEFERRED.
CLASS lcl_sdk_zipfile_collection DEFINITION DEFERRED.
CLASS lcl_sdk_internet_manager DEFINITION DEFERRED.
CLASS lcl_sdk_report_update_manager DEFINITION DEFERRED.
CLASS lcl_sdk_certificate_manager DEFINITION DEFERRED.
CLASS lcl_sdk_file_manager DEFINITION DEFERRED.
CLASS lcl_sdk_transport_manager DEFINITION DEFERRED.
CLASS lcl_sdk_job_manager DEFINITION DEFERRED.
CLASS lcl_sdk_package_manager DEFINITION DEFERRED.

CLASS lcl_ui_tree_controller DEFINITION DEFERRED.

CLASS lcl_utils DEFINITION DEFERRED.
CLASS lcx_error DEFINITION DEFERRED.
CLASS lcl_main DEFINITION DEFERRED.


INTERFACE lif_sdk_constants.
  CONSTANTS:
    c_trans_logical_path      TYPE filepath-pathintern VALUE 'ASSEMBLY' ##NO_TEXT,
    c_logsubdir_name          TYPE fileintern VALUE 'BC_RSTEXTA3' ##NO_TEXT,
    c_download_uri_prefix     TYPE string VALUE '://sdk-for-sapabap.aws.amazon.com/awsSdkSapabapV' ##NO_TEXT,
    c_sdk_inst_file_prefix    TYPE string VALUE 'abapsdk-' ##NO_TEXT,
    c_sdk_uninst_file_prefix  TYPE string VALUE 'uninstall-abapsdk-' ##NO_TEXT,
    c_sdk_index_json_zip_path TYPE string VALUE 'META-INF/sdk_index.json' ##NO_TEXT,
    c_transports_zip_path     TYPE string VALUE 'transports/' ##NO_TEXT,
    c_url_internet_check      TYPE w3_url VALUE 'http://ocsp.r2m02.amazontrust.com' ##NO_TEXT,
    c_url_github_raw          TYPE w3_url VALUE 'https://raw.githubusercontent.com/awslabs/gui-installer-for-sap-abap-sdk/refs/heads/main/src/%23awslabs%23sdk_installer.prog.abap'  ##NO_TEXT.
ENDINTERFACE.

INTERFACE lif_ui_constants.
  CONSTANTS: c_operation_none              TYPE i VALUE 0,
             c_operation_install           TYPE i VALUE 1,
             c_operation_delete            TYPE i VALUE 2,
             c_operation_update            TYPE i VALUE 3,
             c_batch_rec_threshold         TYPE i VALUE 5,
             c_sapgui_autologout_threshold TYPE i VALUE 900.
ENDINTERFACE.





TYPES: BEGIN OF ts_sdk_module,
         tla        TYPE c LENGTH 4,   " Three-letter-acronym (sometimes two-letter only)
         name       TYPE string,       " Human-readable name
         cvers      TYPE string,       " Currently installed version
         ctransport TYPE string,       " Currently installed transport id
         tp_rc      TYPE string,       " TP returncode
         tp_icon    TYPE string,       " TP icon indicating the state of the transport
         tp_text    TYPE string,       " TP human-readable status text
         avers      TYPE string,       " Available version
         atransport TYPE string,       " Available transport id
         is_core    TYPE boolean,      " Is this a core module?
         is_popular TYPE boolean,      " Is this part of the popular modules group?
         op_icon    TYPE string,       " Operation icon for ALV Tree View
         op_text    TYPE string,       " Human-readable operation text
         op_code    TYPE string,       " What will be happening after user hits execute
       END OF ts_sdk_module.

" https://community.sap.com/t5/application-development-discussions/the-type-of-returning-parameter-must-be-fully-specified/td-p/1731970
TYPES: tt_sdk_module TYPE STANDARD TABLE OF ts_sdk_module WITH KEY tla.

TYPES: BEGIN OF ts_sdk_tla,
         tla     TYPE c LENGTH 4,
         version TYPE string,
       END OF ts_sdk_tla.

TYPES tt_sdk_tla TYPE SORTED TABLE OF ts_sdk_tla WITH UNIQUE KEY tla.


CLASS lcx_error DEFINITION INHERITING FROM cx_static_check.
  PUBLIC SECTION.
    DATA av_msg TYPE string.
    DATA av_typ TYPE syst_msgty.
    DATA av_dsp TYPE syst_msgty.
    METHODS constructor
      IMPORTING
        iv_msg   TYPE string
        iv_typ   TYPE syst_msgty DEFAULT 'I'
        iv_dsp   TYPE syst_msgty DEFAULT 'E'
        previous TYPE REF TO cx_root OPTIONAL.
    METHODS show.
ENDCLASS.


CLASS lcx_error IMPLEMENTATION.
  METHOD constructor.
    super->constructor( previous = previous ).
    av_msg = iv_msg.
    av_typ = iv_typ.
    av_dsp = iv_dsp.
  ENDMETHOD.

  METHOD show.
    MESSAGE av_msg TYPE av_typ DISPLAY LIKE av_dsp.
  ENDMETHOD.

ENDCLASS.

CLASS lcl_utils DEFINITION FINAL.

  PUBLIC SECTION.

    CLASS-METHODS:
      cmp_version_string IMPORTING i_string1       TYPE string
                                   i_string2       TYPE string
                         RETURNING VALUE(r_result) TYPE i,
      get_client_role IMPORTING i_client        TYPE mandt
                      RETURNING VALUE(r_result) TYPE cccategory,
      get_system_name RETURNING VALUE(r_sysnam) TYPE tmssysnam,
      get_parameter IMPORTING iv_param        TYPE string
                    RETURNING VALUE(rv_value) TYPE string
                    RAISING   lcx_error,
      has_prod_client RETURNING VALUE(r_result) TYPE abap_bool,
      binary_to_xstring IMPORTING it_bintab         TYPE table
                                  iv_length         TYPE i
                        RETURNING VALUE(ov_xstring) TYPE xstring
                        RAISING   lcx_error,
      raise_lpath_exception IMPORTING i_filename TYPE string
                            RAISING   lcx_error.
ENDCLASS.


CLASS lcl_utils IMPLEMENTATION.


  METHOD cmp_version_string.

    IF i_string1 CO '0123456789.' AND
       i_string2 CO '0123456789.' AND
       i_string1 CP '*.*.*' AND
       i_string2 CP '*.*.*'.
      SPLIT i_string1 AT '.' INTO TABLE DATA(lt_string1).
      SPLIT i_string2 AT '.' INTO TABLE DATA(lt_string2).

      DATA(l_result) = 0.

      DATA wa_string1 TYPE i.
      DATA wa_string2 TYPE i.

      DO lines( lt_string1 ) TIMES.

        wa_string1 = lt_string1[ sy-index ].
        wa_string2 = lt_string2[ sy-index ].

        IF wa_string1 > wa_string2.
          l_result = 1.
          EXIT.
        ELSEIF wa_string1 < wa_string2.
          l_result = -1.
          EXIT.
        ELSE.
          l_result = 0.
        ENDIF.

      ENDDO.
    ELSE.

      l_result = -1.
    ENDIF.

    r_result = l_result.

  ENDMETHOD.


  METHOD get_client_role.
    SELECT SINGLE cccategory INTO @r_result FROM t000 AS t WHERE t~mandt = @i_client.
    IF sy-subrc <> 0.
      MESSAGE |Could not read client role from T000| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDIF.

  ENDMETHOD.

  METHOD get_system_name.

    CALL FUNCTION 'GET_SYSTEM_NAME'
      IMPORTING
        system_name = r_sysnam.

  ENDMETHOD.


  "check if at least one client in the system has role "Production"
  METHOD has_prod_client.

    SELECT cccategory INTO TABLE @DATA(it_t000) FROM t000 AS t.
    IF sy-subrc <> 0.
      MESSAGE |Could not read client category from T000| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDIF.

    IF line_exists( it_t000[ cccategory = 'P' ] ).
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD binary_to_xstring.
    CALL FUNCTION 'SCMS_BINARY_TO_XSTRING'
      EXPORTING
        input_length = iv_length
      IMPORTING
        buffer       = ov_xstring
      TABLES
        binary_tab   = it_bintab
      EXCEPTIONS
        failed       = 1
        OTHERS       = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not convert bin table to xstring| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

  METHOD get_parameter.
    DATA lv_value TYPE pfepvalue.
    DATA lv_rc TYPE i.


    CALL FUNCTION 'TH_GET_PARAMETER'
      EXPORTING
        parameter_name  = CONV pfeparname( iv_param )
      IMPORTING
        parameter_value = lv_value
        rc              = lv_rc
      EXCEPTIONS
        not_authorized  = 1
        OTHERS          = 2.

    IF lv_rc <> 0 OR sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not determine parameter { iv_param }| ##NO_TEXT.
    ENDIF.
    rv_value = lv_value.
  ENDMETHOD.

  METHOD raise_lpath_exception.
    RAISE EXCEPTION TYPE lcx_error
      EXPORTING
        iv_msg = |Could not determine location to save { i_filename }. | &&
                 |Please maintain logical path { lif_sdk_constants=>c_trans_logical_path } in transaction FILE | &&
                 |to point to physical path <P=DIR_TRANS>/<PARAM_1>/<FILENAME>| ##NO_TEXT.
  ENDMETHOD.

ENDCLASS.



INTERFACE lif_sdk_internet_manager.

  METHODS:
    download IMPORTING i_absolute_uri         TYPE w3_url
                       i_blankstocrlf         TYPE boolean
             RETURNING VALUE(r_response_body) TYPE xstring
             RAISING   lcx_error,
    has_internet_access
      RETURNING VALUE(r_result) TYPE abap_bool
      RAISING   lcx_error.

ENDINTERFACE.


CLASS lcl_sdk_internet_manager DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_sdk_internet_manager.

ENDCLASS.

CLASS lcl_sdk_internet_manager IMPLEMENTATION.

  METHOD lif_sdk_internet_manager~download.

    DATA status_code TYPE i.
    DATA status_text TYPE string.
    DATA response_entity_body_length TYPE i.

    DATA request_entity_body TYPE STANDARD TABLE OF docs.
    DATA request_headers TYPE STANDARD TABLE OF docs.
    DATA response_entity_body TYPE STANDARD TABLE OF docs.
    DATA response_headers TYPE STANDARD TABLE OF docs.

    DATA wa_response_entity_body TYPE docs.


    cl_http_client=>create_by_url( EXPORTING url = CONV string( i_absolute_uri )
                                             ssl_id = 'DFAULT'  " use SSLC
                                   IMPORTING client = DATA(http_client) ).
    http_client->request->set_method( if_http_request=>co_request_method_get ).
    http_client->send( EXPORTING  timeout                    = if_http_client=>co_timeout_default
                       EXCEPTIONS http_communication_failure = 1
                                  http_invalid_state         = 2
                                  http_processing_failed     = 3
                                  http_invalid_timeout       = 4
                                  OTHERS                     = 99 ).
    IF sy-subrc = 0.

      http_client->receive( EXCEPTIONS http_communication_failure = 1
                                       http_invalid_state         = 2
                                       http_processing_failed     = 3
                                       OTHERS                     = 99 ).
    ENDIF.
    IF sy-subrc > 0.
      CASE sy-subrc.
        WHEN 1.
          DATA(msg) = |HTTP communication error|.
        WHEN 2.
          msg = |HTTP invalid state|.
        WHEN 3.
          msg = |HTTP processing failed|.
        WHEN 4.
          msg = |HTTP invalid timeout|.
        WHEN OTHERS.
          msg = |Unknown error|.
      ENDCASE.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_msg = |Could not establish a connection to { i_absolute_uri }:| &&
                   msg && |Please check the SMICM trace for more details.| ##NO_TEXT.
    ENDIF.


    DATA(response) = http_client->response.
    response->get_status( IMPORTING code = status_code reason = status_text ).
    r_response_body = response->get_data( ).

    IF status_code >= 299.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_msg = |HTTP error { status_text }({ status_code }) when attempting GET from { i_absolute_uri }.| &&
                   |Please check the SMICM log for SSL errors such as untrusted certificates| ##NO_TEXT.
    ENDIF.

  ENDMETHOD.

  METHOD lif_sdk_internet_manager~has_internet_access.

    DATA result TYPE abap_bool.
    DATA http_client TYPE REF TO if_http_client.
    DATA reason TYPE string.
    DATA url TYPE string.
    DATA http_rc TYPE i.
    DATA status_text TYPE string.

    result = abap_false.

    url = lif_sdk_constants=>c_url_internet_check.

    TRY.
        cl_http_client=>create_by_url( EXPORTING url = url IMPORTING client = http_client ).

        http_client->request->set_method( if_http_request=>co_request_method_get ).

        http_client->send( timeout = if_http_client=>co_timeout_default ).

        http_client->receive( EXCEPTIONS http_communication_failure = 1
                                         http_invalid_state         = 2
                                         http_processing_failed     = 3
                                         OTHERS                     = 4 ).

        IF sy-subrc <> 0.
          result = abap_false.
        ELSE.
          result = abap_true.
        ENDIF.

      CATCH cx_root INTO DATA(ex).
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = ex->get_text( ).
    ENDTRY.

    r_result = result.
  ENDMETHOD.

ENDCLASS.


INTERFACE lif_sdk_report_update_manager.

  METHODS:
    is_update_available RETURNING VALUE(r_result) TYPE boolean
                        RAISING   lcx_error,
    update_report RAISING lcx_error.

ENDINTERFACE.


CLASS lcl_sdk_report_update_manager DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_sdk_report_update_manager.

    METHODS:
      constructor IMPORTING i_sdk_internet_manager TYPE REF TO lif_sdk_internet_manager OPTIONAL.

  PRIVATE SECTION.
    DATA: internet_manager TYPE REF TO lif_sdk_internet_manager.
    DATA: report_github_cached TYPE xstring.

    METHODS:
      download_report IMPORTING i_url_github_raw        TYPE w3_url
                      RETURNING VALUE(r_report_xstring) TYPE xstring
                      RAISING   lcx_error,
      write_report_update RAISING   lcx_error.

ENDCLASS.


CLASS lcl_sdk_report_update_manager IMPLEMENTATION.


  METHOD constructor.
    IF i_sdk_internet_manager IS BOUND.
      internet_manager = i_sdk_internet_manager.
    ELSE.
      internet_manager = NEW lcl_sdk_internet_manager( ).
    ENDIF.
  ENDMETHOD.

  METHOD lif_sdk_report_update_manager~update_report.
    IF report_github_cached IS NOT INITIAL.
      write_report_update( ).
    ELSE.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Check for update first!| ##NO_TEXT..
    ENDIF.
  ENDMETHOD.


  METHOD lif_sdk_report_update_manager~is_update_available.

    DATA: report_local TYPE xstring.
    DATA: report_local_stringtab TYPE TABLE OF string.
    DATA: report_local_string TYPE string.

    " retrieve current version from github, store it in private instance variable
    TRY.
        report_github_cached = download_report( i_url_github_raw = lif_sdk_constants=>c_url_github_raw ).
      CATCH lcx_error INTO DATA(ex1).
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = ex1->get_text( ).
    ENDTRY.

    " read local report version as xstring
    TRY.
        READ REPORT sy-repid INTO report_local_stringtab.
        CONCATENATE LINES OF report_local_stringtab INTO report_local_string SEPARATED BY cl_abap_char_utilities=>newline.
        report_local = cl_abap_conv_codepage=>create_out( )->convert( source = report_local_string ).
      CATCH cx_root INTO DATA(ex2).
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = ex2->get_text( ).
    ENDTRY.

    r_result = xsdbool( report_local <> report_github_cached ).

  ENDMETHOD.


  METHOD download_report.
    TRY.
        r_report_xstring = internet_manager->download( i_absolute_uri = i_url_github_raw
                                                       i_blankstocrlf = abap_false ).
      CATCH lcx_error INTO DATA(ex).
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = ex->get_text( ).
    ENDTRY.
  ENDMETHOD.


  METHOD write_report_update.

    " Safety check, if the cached version is still initial, do not overwrite
    IF report_github_cached IS INITIAL.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Report content is empty!| ##NO_TEXT..
    ENDIF.

    TRY.
        DATA(report_string) = cl_abap_conv_codepage=>create_in( )->convert( source = report_github_cached ).
      CATCH cx_root INTO DATA(ex).
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = ex->get_text( ).
    ENDTRY.

    SPLIT report_string AT cl_abap_char_utilities=>newline INTO TABLE DATA(report_stringtab).

    INSERT REPORT sy-repid FROM report_stringtab.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Failed to write report update!| ##NO_TEXT..
    ENDIF.


  ENDMETHOD.

ENDCLASS.


INTERFACE lif_sdk_certificate_manager.

  TYPES: BEGIN OF ts_amazon_root_certificate,
           subject   TYPE string,
           uri       TYPE w3_url,
           binary    TYPE xstring,
           installed TYPE abap_bool,
         END OF ts_amazon_root_certificate.

  TYPES: tt_amazon_root_certificate TYPE STANDARD TABLE OF ts_amazon_root_certificate WITH KEY subject.

  METHODS:

    install_amazon_root_certs RETURNING VALUE(r_success) TYPE abap_bool RAISING lcx_error.

ENDINTERFACE.


CLASS lcl_sdk_certificate_manager DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_sdk_certificate_manager.

    METHODS:
      constructor IMPORTING i_internet_manager TYPE REF TO lif_sdk_internet_manager OPTIONAL RAISING lcx_error.


  PRIVATE SECTION.
    DATA: internet_manager TYPE REF TO lif_sdk_internet_manager.
    DATA: mt_amazon_root_certs TYPE lif_sdk_certificate_manager~tt_amazon_root_certificate. " populated in set_amazon_root_cert_values

    METHODS:
      set_amazon_root_cert_values,
      set_amazon_root_cert_status RAISING lcx_error,
      retrieve_missing_certificates RETURNING VALUE(r_result) TYPE abap_bool RAISING lcx_error,
      check_bapiret IMPORTING it_bapiret2 TYPE bapiret2_t
                    RAISING   lcx_error.

ENDCLASS.


CLASS lcl_sdk_certificate_manager IMPLEMENTATION.

  METHOD constructor.

    IF i_internet_manager IS BOUND.
      internet_manager = i_internet_manager.
    ELSE.
      internet_manager = NEW lcl_sdk_internet_manager( ).
    ENDIF.

    set_amazon_root_cert_values( ). " Sets certificate URLs in member table

    set_amazon_root_cert_status( ). " Sets the certificates' status to installed/not installed

    retrieve_missing_certificates( ).

  ENDMETHOD.



  METHOD retrieve_missing_certificates.

    DATA lt_missing_certs TYPE lif_sdk_certificate_manager~tt_amazon_root_certificate.
    LOOP AT mt_amazon_root_certs INTO DATA(wa_amazon_root_cert).
      IF wa_amazon_root_cert-installed = abap_false AND sy-tabix > 4. " Certs that are mentioned in the ABAP SDK docs appear after tabix 4
        APPEND wa_amazon_root_cert TO lt_missing_certs.
      ENDIF.
    ENDLOOP.

    IF lines( lt_missing_certs ) > 0.

      DATA lv_text TYPE string.
      lv_text = |It appears your system is missing one or more of the five Amazon Root SSL Certificates. |
                && |These certificates have to be installed in order to download and use the newest version of the ABAP SDK. |
                && |The report can download and maintain them automatically for you if your SAP system has an Internet connection. |
                && |Amazon Root SSL Certificates are available from https://www.amazontrust.com/repository/ | ##NO_TEXT.

      DATA lv_answer TYPE c.

      CALL FUNCTION 'POPUP_TO_CONFIRM'
        EXPORTING
          titlebar              = ' '
          diagnose_object       = ' '
          text_question         = lv_text
          text_button_1         = |Do it for me|
          icon_button_1         = ' '
          text_button_2         = |Not now|
          icon_button_2         = ' '
          default_button        = '1'
          display_cancel_button = ' '
          userdefined_f1_help   = ' '
          start_column          = 25
          start_row             = 6
          iv_quickinfo_button_1 = ' '
          iv_quickinfo_button_2 = ' '
        IMPORTING
          answer                = lv_answer
        EXCEPTIONS
          text_not_found        = 1
          OTHERS                = 2 ##NO_TEXT.
      IF sy-subrc <> 0.
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not find text for popup| ##NO_TEXT.
      ENDIF.

      IF lv_answer = 1.
        lif_sdk_certificate_manager~install_amazon_root_certs( ).
      ELSE.
        LEAVE PROGRAM.
      ENDIF.
    ENDIF.

  ENDMETHOD.


  METHOD set_amazon_root_cert_values.
    INSERT VALUE #( subject = 'CN=Amazon RSA 2048 M01, O=Amazon, C=US'
                    uri = 'http://crt.r2m01.amazontrust.com/r2m01.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 1 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon RSA 2048 M02, O=Amazon, C=US'
                    uri = 'http://crt.r2m01.amazontrust.com/r2m02.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 2 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon RSA 2048 M03, O=Amazon, C=US'
                    uri = 'http://crt.r2m01.amazontrust.com/r2m03.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 3 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon RSA 2048 M04, O=Amazon, C=US'
                    uri = 'http://crt.r2m01.amazontrust.com/r2m04.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 4 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon Root CA 1, O=Amazon, C=US'
                    uri = 'https://www.amazontrust.com/repository/AmazonRootCA1.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 5 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon Root CA 2, O=Amazon, C=US'
                    uri = 'https://www.amazontrust.com/repository/AmazonRootCA2.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 6 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon Root CA 3, O=Amazon, C=US'
                    uri = 'https://www.amazontrust.com/repository/AmazonRootCA3.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 7 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Amazon Root CA 4, O=Amazon, C=US'
                    uri = 'https://www.amazontrust.com/repository/AmazonRootCA4.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 8 ##NO_TEXT.
    INSERT VALUE #( subject = 'CN=Starfield Services Root Certificate Authority - G2, O="Starfield Technologies, Inc.", L=Scottsdale, SP=Arizona, C=US'
                    uri = 'https://www.amazontrust.com/repository/SFSRootCAG2.cer'
                    binary = ''
                    installed = abap_false ) INTO me->mt_amazon_root_certs INDEX 9 ##NO_TEXT.
  ENDMETHOD.


  METHOD set_amazon_root_cert_status.

    DATA ls_strust_identity TYPE ssf_s_strust_identity.
    DATA lt_certificatelist TYPE ssfbintab.
    DATA wa_certificate_binary TYPE LINE OF ssfbintab.
    DATA wa_amazon_root_cert TYPE lif_sdk_certificate_manager~ts_amazon_root_certificate.
    DATA lt_bapiret2 TYPE STANDARD TABLE OF bapiret2.


    ls_strust_identity-pse_context = 'SSLC'.
    ls_strust_identity-pse_applic = 'DFAULT'.
    ls_strust_identity-pse_descript = 'SSL Client (Standard)' ##NO_TEXT.
    ls_strust_identity-sprsl = 'E'.


    CALL FUNCTION 'SSFR_GET_CERTIFICATELIST'
      EXPORTING
        is_strust_identity = ls_strust_identity
      IMPORTING
        et_certificatelist = lt_certificatelist
      TABLES
        et_bapiret2        = lt_bapiret2.
    check_bapiret( lt_bapiret2 ).

    CLEAR lt_bapiret2.

    DATA l_subject TYPE string.
    DATA l_issuer TYPE string.
    DATA l_serialno TYPE string.
    DATA l_validfrom TYPE string.
    DATA l_validto TYPE string.
    DATA l_algid TYPE string.

    LOOP AT lt_certificatelist INTO wa_certificate_binary.

      CALL FUNCTION 'SSFR_PARSE_CERTIFICATE'
        EXPORTING
          iv_certificate = wa_certificate_binary
        IMPORTING
          ev_subject     = l_subject
          ev_issuer      = l_issuer
          ev_serialno    = l_serialno
          ev_validfrom   = l_validfrom
          ev_validto     = l_validto
          ev_algid       = l_algid
        TABLES
          et_bapiret2    = lt_bapiret2.
      check_bapiret( lt_bapiret2 ).

      READ TABLE mt_amazon_root_certs WITH TABLE KEY subject = l_subject INTO wa_amazon_root_cert.

      IF sy-subrc = 0.  " found existing certificate
        wa_amazon_root_cert-binary = wa_certificate_binary.
        wa_amazon_root_cert-installed = abap_true.
        MODIFY TABLE mt_amazon_root_certs FROM wa_amazon_root_cert.
      ENDIF.

      CLEAR: l_subject, l_issuer, l_serialno, l_validfrom, l_validto, l_algid.
      CLEAR wa_amazon_root_cert.
      CLEAR lt_bapiret2.
    ENDLOOP.

  ENDMETHOD.


  METHOD lif_sdk_certificate_manager~install_amazon_root_certs.

    DATA wa_amazon_root_cert TYPE lif_sdk_certificate_manager~ts_amazon_root_certificate.

    DATA ls_strust_identity TYPE ssf_s_strust_identity.
    DATA lt_bapiret2 TYPE STANDARD TABLE OF bapiret2.

    DATA l_abap_matcher TYPE REF TO cl_abap_matcher.
    DATA l_abap_regex_pcre TYPE REF TO cl_abap_regex.


    ls_strust_identity-pse_context = 'SSLC' ##NO_TEXT.
    ls_strust_identity-pse_applic = 'DFAULT' ##NO_TEXT.
    ls_strust_identity-pse_descript = 'SSL Client (Standard)' ##NO_TEXT.
    ls_strust_identity-sprsl = 'E'.

    " root cert URLS set in set_amazon_root_cert_values
    LOOP AT mt_amazon_root_certs INTO wa_amazon_root_cert FROM 1 TO 4.

      IF wa_amazon_root_cert-installed = abap_true.
        CONTINUE.
      ENDIF.

      DATA(root_cert_name_http) = segment( val   = wa_amazon_root_cert-uri
                                           sep   = '/'
                                           index = -1 ).
      wa_amazon_root_cert-binary = internet_manager->download( i_absolute_uri = wa_amazon_root_cert-uri
                                                               i_blankstocrlf = abap_false ).
      CALL FUNCTION 'SSFR_PUT_CERTIFICATE'
        EXPORTING
          is_strust_identity = ls_strust_identity
          iv_certificate     = wa_amazon_root_cert-binary
        TABLES
          et_bapiret2        = lt_bapiret2.
      check_bapiret( lt_bapiret2 ).

    ENDLOOP.

    CALL FUNCTION 'ICM_SSL_PSE_CHANGED'
      EXCEPTIONS
        icm_op_failed       = 1
        icm_get_serv_failed = 2
        icm_auth_failed     = 3
        OTHERS              = 4.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Error updating PSE| ##NO_TEXT.
    ENDIF.

    " root cert URLS set in set_amazon_root_cert_values
    LOOP AT mt_amazon_root_certs INTO wa_amazon_root_cert FROM 5.

      IF wa_amazon_root_cert-installed = abap_true.
        CONTINUE.
      ENDIF.

      DATA(root_cert_name_https) = segment( val   = wa_amazon_root_cert-uri
                                            sep   = '/'
                                            index = -1 ).
      wa_amazon_root_cert-binary = internet_manager->download( i_absolute_uri = wa_amazon_root_cert-uri
                                                               i_blankstocrlf = abap_false ).
      CALL FUNCTION 'SSFR_PUT_CERTIFICATE'
        EXPORTING
          is_strust_identity = ls_strust_identity
          iv_certificate     = wa_amazon_root_cert-binary
        TABLES
          et_bapiret2        = lt_bapiret2.
      check_bapiret( lt_bapiret2 ).

    ENDLOOP.

    MESSAGE |Certificates installed.| TYPE 'S' ##NO_TEXT.

    r_success = abap_true .

  ENDMETHOD.


  METHOD check_bapiret.
    LOOP AT it_bapiret2 ASSIGNING FIELD-SYMBOL(<bapiret>) WHERE type CA 'AE'.
      IF <bapiret>-message IS NOT INITIAL.
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not put certificate: { <bapiret>-message }| ##NO_TEXT.
      ELSE.
        DATA lv_msg TYPE string.
        MESSAGE ID <bapiret>-id TYPE <bapiret>-type NUMBER <bapiret>-number
          WITH <bapiret>-message_v1 <bapiret>-message_v2 <bapiret>-message_v3 <bapiret>-message_v4
          INTO lv_msg.
        RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not put certificate: { lv_msg }| ##NO_TEXT.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


ENDCLASS.


INTERFACE lif_sdk_file_manager.

  METHODS:
    get_file_from_zip IMPORTING i_zipfile_absolute_path TYPE string
                                i_file_to_retrieve      TYPE string
                      RETURNING VALUE(r_file_xstring)   TYPE xstring,
    check_file_writable_at IMPORTING i_path          TYPE string
                           RETURNING VALUE(r_result) TYPE abap_bool,
    check_file_exists_at IMPORTING i_path          TYPE string
                         RETURNING VALUE(r_result) TYPE abap_bool,
    open_for_input
      IMPORTING iv_filename TYPE clike
      RAISING   lcx_error,
    open_for_output
      IMPORTING iv_filename TYPE clike
      RAISING   lcx_error.

ENDINTERFACE.

CLASS lcl_sdk_file_manager DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_sdk_file_manager.

ENDCLASS.

CLASS lcl_sdk_file_manager IMPLEMENTATION.

  METHOD lif_sdk_file_manager~get_file_from_zip.
    DATA lv_zipfile_content TYPE xstring.
    TRY.
        lif_sdk_file_manager~open_for_input( i_zipfile_absolute_path ).
        READ DATASET i_zipfile_absolute_path INTO lv_zipfile_content.
        CLOSE DATASET i_zipfile_absolute_path.
      CATCH cx_sy_file_authority INTO DATA(r_ex1).
        MESSAGE r_ex1->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      CATCH cx_sy_file_access_error INTO DATA(r_ex2).
        MESSAGE r_ex2->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      CATCH cx_root INTO DATA(r_ex).
        MESSAGE r_ex->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    DATA(r_zip) = NEW cl_abap_zip( ).

    r_zip->load( zip = lv_zipfile_content ).

    r_zip->get( EXPORTING  name                    = i_file_to_retrieve
                IMPORTING  content                 = r_file_xstring
                EXCEPTIONS zip_index_error         = 1
                           zip_decompression_error = 2
                           OTHERS                  = 3 ).

    CASE sy-subrc.
      WHEN 0.
        " success
      WHEN 1.
        MESSAGE 'Zip Index Error' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
        RETURN.
      WHEN 2.
        MESSAGE 'Zip Decompression Error' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
        RETURN.
      WHEN OTHERS.
        MESSAGE 'Unknown Zip error' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
        RETURN.
    ENDCASE.
  ENDMETHOD.


  METHOD lif_sdk_file_manager~check_file_writable_at.
    TRY.
        lif_sdk_file_manager~open_for_output( i_path ).
        CLOSE DATASET i_path.
        r_result = abap_true.
      CATCH lcx_error INTO DATA(lo_ex).
        MESSAGE |Path { i_path } not writable, reason: { lo_ex->av_msg }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDTRY.
  ENDMETHOD.


  METHOD lif_sdk_file_manager~check_file_exists_at.

    DATA(lv_path) = CONV pfebackuppro( i_path ).
    CALL FUNCTION 'PFL_CHECK_OS_FILE_EXISTENCE'
      EXPORTING
        long_filename         = lv_path
      IMPORTING
        file_exists           = r_result
      EXCEPTIONS
        authorization_missing = 1
        OTHERS                = 2.

    CASE sy-subrc.
      WHEN 0.
        " all good
      WHEN 1.
        RAISE EXCEPTION TYPE cx_sy_file_authority.
      WHEN 2.
        RAISE EXCEPTION TYPE cx_sy_file_io. " TODO: Find/Create more appropriate exception for generic case
    ENDCASE.

  ENDMETHOD.


  METHOD lif_sdk_file_manager~open_for_input.
    DATA lv_os_msg TYPE text255.
    OPEN DATASET iv_filename FOR INPUT IN BINARY MODE MESSAGE lv_os_msg.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_msg = |Error opening { iv_filename }: { lv_os_msg }| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.


  METHOD lif_sdk_file_manager~open_for_output.
    DATA(lv_filename_to_check) = CONV fileextern( iv_filename ).
    AUTHORITY-CHECK OBJECT 'S_DATASET'
      ID 'PROGRAM' FIELD sy-repid
      ID 'ACTVT' FIELD '34'
      ID 'FILENAME' FIELD lv_filename_to_check ##AUTH_FLD_LEN.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_msg = |No permission to write to { iv_filename }| ##NO_TEXT.
    ENDIF.

    DATA lv_os_msg TYPE text255.
    OPEN DATASET iv_filename FOR OUTPUT IN BINARY MODE MESSAGE lv_os_msg.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error
        EXPORTING
          iv_msg = |Error opening { iv_filename }: { lv_os_msg }| ##NO_TEXT.
    ENDIF.
  ENDMETHOD.

ENDCLASS.


INTERFACE lif_sdk_transport_manager.

  METHODS:
    get_sdk_cofile_from_zip IMPORTING i_tla                   TYPE ts_sdk_module-tla
                                      i_transport             TYPE ts_sdk_module-atransport
                                      i_operation             TYPE string
                                      i_version               TYPE string
                                      i_zipfile_absolute_path TYPE string
                            EXPORTING e_cofile_name           TYPE string
                                      e_cofile_blob           TYPE xstring,
    get_sdk_datafile_from_zip IMPORTING i_tla                   TYPE ts_sdk_module-tla
                                        i_transport             TYPE ts_sdk_module-atransport
                                        i_operation             TYPE string
                                        i_version               TYPE string
                                        i_zipfile_absolute_path TYPE string
                              EXPORTING e_datafile_name         TYPE string
                                        e_datafile_blob         TYPE xstring,
    write_sdk_transport_trdir IMPORTING i_cofile_name    TYPE string
                                        i_datafile_name  TYPE string
                                        i_cofile_blob    TYPE xstring
                                        i_datafile_blob  TYPE xstring
                              RETURNING VALUE(r_success) TYPE boolean
                              RAISING   lcx_error,
    import_sdk_transports IMPORTING it_transport_names TYPE stms_tr_requests
                          EXPORTING e_tp_retcode       TYPE stpa-retcode
                                    es_exception       TYPE stmscalert.

ENDINTERFACE.

CLASS lcl_sdk_transport_manager DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_sdk_transport_manager.

    METHODS:
      constructor IMPORTING i_file_manager TYPE REF TO lcl_sdk_file_manager OPTIONAL.

  PRIVATE SECTION.
    DATA:
       file_manager TYPE REF TO lcl_sdk_file_manager.

ENDCLASS.

CLASS lcl_sdk_transport_manager IMPLEMENTATION.

  METHOD constructor.

    IF i_file_manager IS BOUND.
      file_manager = i_file_manager.
    ELSE.
      file_manager = NEW lcl_sdk_file_manager( ).
    ENDIF.

  ENDMETHOD.

  METHOD lif_sdk_transport_manager~write_sdk_transport_trdir.

    DATA lv_rc TYPE i.
    DATA lv_validation_active TYPE abap_bool.
    DATA lv_filepath_cofile TYPE string.
    DATA lv_filepath_datafile TYPE string.
    DATA gv_file_name(255) TYPE c.



    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        client                     = sy-mandt
        logical_path               = lif_sdk_constants=>c_trans_logical_path
        operating_system           = sy-opsys
        parameter_1                = 'cofiles'
        file_name                  = i_cofile_name
      IMPORTING
        file_name_with_path        = lv_filepath_cofile
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      lcl_utils=>raise_lpath_exception( i_cofile_name ).
    ENDIF.


    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        logical_filename  = lif_sdk_constants=>c_logsubdir_name
        parameter_1       = 'cofiles'
      CHANGING
        physical_filename = lv_filepath_cofile
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.



    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        client                     = sy-mandt
        logical_path               = lif_sdk_constants=>c_trans_logical_path
        operating_system           = sy-opsys
        parameter_1                = 'data'
        file_name                  = i_datafile_name
      IMPORTING
        file_name_with_path        = lv_filepath_datafile
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      lcl_utils=>raise_lpath_exception( i_datafile_name ).
    ENDIF.

    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        logical_filename  = lif_sdk_constants=>c_logsubdir_name
        parameter_1       = 'data'
      CHANGING
        physical_filename = lv_filepath_datafile
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.


    TRY.
        file_manager->lif_sdk_file_manager~open_for_output( lv_filepath_cofile ).
        TRANSFER i_cofile_blob TO lv_filepath_cofile.
        CLOSE DATASET lv_filepath_cofile.

        file_manager->lif_sdk_file_manager~open_for_output( lv_filepath_datafile ).
        TRANSFER i_datafile_blob TO lv_filepath_datafile.
        CLOSE DATASET lv_filepath_datafile.
      CATCH cx_sy_file_authority INTO DATA(r_ex1).
        MESSAGE r_ex1->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      CATCH cx_sy_file_access_error INTO DATA(r_ex2).
        MESSAGE r_ex2->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
      CATCH cx_root INTO DATA(r_ex).
        MESSAGE r_ex->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
        RETURN.
    ENDTRY.

    r_success = abap_true.

  ENDMETHOD.


  METHOD lif_sdk_transport_manager~get_sdk_cofile_from_zip.

    DATA l_cofile_zip_path TYPE string.
    DATA l_cofile_name TYPE string.
    DATA l_cofile_blob TYPE xstring.

    e_cofile_name = 'K' && i_transport+4 && '.' && i_transport+0(3).

    l_cofile_zip_path = lif_sdk_constants=>c_transports_zip_path && i_tla && '/' && e_cofile_name.

    e_cofile_blob = file_manager->lif_sdk_file_manager~get_file_from_zip( i_zipfile_absolute_path = i_zipfile_absolute_path
                                                                          i_file_to_retrieve      = l_cofile_zip_path ).

  ENDMETHOD.


  METHOD lif_sdk_transport_manager~get_sdk_datafile_from_zip.

    DATA l_datafile_zip_path TYPE string.
    DATA l_datafile_name TYPE string.
    DATA l_datafile_blob TYPE xstring.


    e_datafile_name = 'R' && i_transport+4 && '.' && i_transport+0(3).

    l_datafile_zip_path = lif_sdk_constants=>c_transports_zip_path && i_tla && '/' && e_datafile_name.

    e_datafile_blob = file_manager->lif_sdk_file_manager~get_file_from_zip( i_zipfile_absolute_path = i_zipfile_absolute_path
                                                                            i_file_to_retrieve      = l_datafile_zip_path ).


  ENDMETHOD.



  METHOD lif_sdk_transport_manager~import_sdk_transports.

    DATA: l_system TYPE tmssysnam.
    DATA: l_tp_ret_code TYPE stpa-retcode.
    DATA: ls_exception TYPE stmscalert.

    l_system = lcl_utils=>get_system_name( ).

    " TODO: Needs to go into its own method
    CALL FUNCTION 'TMS_MGR_FORWARD_TR_REQUEST'
      EXPORTING
        iv_request                 = 'SOME'
        iv_tarcli                  = sy-mandt
        iv_import_again            = abap_true
        iv_target                  = l_system
        it_requests                = it_transport_names
      IMPORTING
        ev_tp_ret_code             = l_tp_ret_code
        es_exception               = ls_exception
      EXCEPTIONS
        read_config_failed         = 1
        table_of_requests_is_empty = 2
        OTHERS                     = 3.
    CASE sy-subrc.
      WHEN 0. " all good
      WHEN 2. " empty import list
      WHEN 4. " finished with warnings
      WHEN OTHERS.
        MESSAGE |Error forwarding requests to { l_system }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
    ENDCASE.

    " TODO: DO SOMETHING USEFUL WITH THESE.
    CLEAR: l_tp_ret_code.
    CLEAR: ls_exception.

* WARNING: Turning off offline processing (IV_OFFLINE = abap_false) makes the
* FM call spawn a new logon session for every transport, which can lead to resource
* exhaustion for very large module import numbers (> 200), check also SAP Profile
* parameters rdisp/user_resource_limit and rdisp/tm_max_no
* ADD: Turning on offline processing (IV_OFFLINE = abap_true) apparently leads to the
* background job finishing prematurely, which is not desired.

    IF sy-batch = abap_true.
      CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system                  = l_system
          iv_request                 = 'SOME'
          iv_client                  = sy-mandt
          iv_import_again            = abap_true
          iv_ignore_cvers            = abap_true
          iv_offline                 = abap_false
          iv_monitor                 = abap_false
          it_requests                = it_transport_names
        IMPORTING
          ev_tp_ret_code             = l_tp_ret_code
          es_exception               = ls_exception
        EXCEPTIONS
          read_config_failed         = 1
          table_of_requests_is_empty = 2
          OTHERS                     = 3.
      CASE sy-subrc.
        WHEN 0. " all good
        WHEN 2. " empty import list
        WHEN 4. " finished with warnings
          WHEN OTHERS: .
          MESSAGE |Error importing requests to { l_system }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      ENDCASE.
    ELSE.
      CALL FUNCTION 'TMS_MGR_IMPORT_TR_REQUEST'
        EXPORTING
          iv_system                  = l_system
          iv_request                 = 'SOME'
          iv_client                  = sy-mandt
          iv_import_again            = abap_true
          iv_ignore_cvers            = abap_true
          iv_offline                 = abap_false
          iv_monitor                 = abap_true
          it_requests                = it_transport_names
        IMPORTING
          ev_tp_ret_code             = l_tp_ret_code
          es_exception               = ls_exception
        EXCEPTIONS
          read_config_failed         = 1
          table_of_requests_is_empty = 2
          OTHERS                     = 3.
      CASE sy-subrc.
        WHEN 0. " all good
        WHEN 2. " empty import list
        WHEN 4. " finished with warnings
        WHEN OTHERS.
          MESSAGE |Error importing requests to { l_system }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      ENDCASE.
    ENDIF.

    e_tp_retcode = l_tp_ret_code.
    es_exception = ls_exception.


  ENDMETHOD.


ENDCLASS.


INTERFACE lif_sdk_job_manager.

  METHODS:
    get_running_jobs IMPORTING i_jobname       TYPE syst_repi2
                     RETURNING VALUE(r_result) TYPE tbtcjob_tt,
    is_job_running RETURNING VALUE(r_result) TYPE abap_bool,
    submit_batch_job IMPORTING i_modules_to_be_installed TYPE tt_sdk_tla
                               i_modules_to_be_deleted   TYPE tt_sdk_tla
                               i_target_version          TYPE string
                     RETURNING VALUE(r_result)           TYPE tbtco-jobcount
                     RAISING   lcx_error.

ENDINTERFACE.


CLASS lcl_sdk_job_manager DEFINITION FINAL.

  PUBLIC SECTION.
    INTERFACES:
      lif_sdk_job_manager.

ENDCLASS.


CLASS lcl_sdk_job_manager IMPLEMENTATION.

  METHOD lif_sdk_job_manager~submit_batch_job.

    DATA(lt_modules_to_be_installed) = i_modules_to_be_installed.
    DATA(lt_modules_to_be_deleted) = i_modules_to_be_deleted.
    DATA(lv_target_version) = i_target_version.


    " Export to shared memory and batch job submission need to go together
    DELETE FROM SHARED BUFFER indx(mi) ID 'MOD_INST'.
    EXPORT st_modules_to_be_installed = lt_modules_to_be_installed TO SHARED BUFFER indx(mi) ID 'MOD_INST'.
    DELETE FROM SHARED BUFFER indx(md) ID 'MOD_DELE'.
    EXPORT st_modules_to_be_deleted = lt_modules_to_be_deleted TO SHARED BUFFER indx(md) ID 'MOD_DELE'.
    DELETE FROM SHARED BUFFER indx(tv) ID 'TAR_VERS'.
    EXPORT s_target_version = lv_target_version TO SHARED BUFFER indx(tv) ID 'TAR_VERS'.


    DATA(lo_job) = NEW cl_bp_abap_job( ).
    lo_job->set_name( CONV char32( sy-repid ) ).
    lo_job->set_report( sy-repid ).
    lo_job->if_bp_job_engine~generate_job_count(
      EXCEPTIONS
        cant_create_job  = 1
        invalid_job_data = 2
        jobname_missing  = 3 ).
    IF sy-subrc = 0.
      lo_job->if_bp_job_engine~plan_job_step(
        EXCEPTIONS
          bad_priparams           = 11
          bad_xpgflags            = 12
          invalid_jobdata         = 13
          jobname_missing         = 14
          job_notex               = 15
          job_submit_failed       = 16
          program_missing         = 17
          prog_abap_and_extpg_set = 18 ).
    ENDIF.
    IF sy-subrc = 0.
      lo_job->if_bp_job_engine~start_immediately( EXCEPTIONS cant_start_immediate = 21 invalid_startdate = 22 jobname_missing = 23 job_close_failed = 24 job_nosteps = 25 job_notex = 26 ).
    ENDIF.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Failed to submit job { sy-repid }: subrc = { sy-subrc }| ##NO_TEXT.
    ENDIF.

    r_result = lo_job->jobcount.

  ENDMETHOD.

  METHOD lif_sdk_job_manager~get_running_jobs.
    " --- Job already running?
    CALL FUNCTION 'BP_FIND_JOBS_WITH_PROGRAM'
      EXPORTING
        abap_program_name             = i_jobname
        status                        = 'R'  " Batch job already running in background?
      TABLES
        joblist                       = r_result
      EXCEPTIONS
        no_jobs_found                 = 1
        program_specification_missing = 2
        invalid_dialog_type           = 3
        job_find_canceled             = 4
        OTHERS                        = 5.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDMETHOD.

  METHOD lif_sdk_job_manager~is_job_running.

    DATA lt_joblist TYPE TABLE OF tbtcjob.
    lt_joblist = lif_sdk_job_manager~get_running_jobs( i_jobname = sy-repid ).

    IF lines( lt_joblist ) > 0.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_sdk_zipfile DEFINITION FINAL.

  PUBLIC SECTION.

    DATA:
      op       TYPE string READ-ONLY,
      version  TYPE string READ-ONLY,
      name     TYPE string READ-ONLY,
      uri      TYPE w3_url READ-ONLY,
      path     TYPE string READ-ONLY,
      json_web TYPE w3_url READ-ONLY.

    METHODS:
      constructor IMPORTING i_op       TYPE string
                            i_version  TYPE string
                            i_name     TYPE string OPTIONAL
                            i_uri      TYPE w3_url OPTIONAL
                            i_path     TYPE string OPTIONAL
                            i_json_web TYPE w3_url OPTIONAL
                  RAISING   lcx_error,
      set_op IMPORTING i_op TYPE string,
      set_version IMPORTING i_version TYPE string,
      set_name IMPORTING i_name TYPE string,
      set_uri IMPORTING i_uri TYPE w3_url,
      set_path IMPORTING i_path TYPE string,
      set_json_web IMPORTING i_json_web TYPE w3_url.

  PRIVATE SECTION.
    METHODS:
      build_download_uri_prefix IMPORTING i_protocol      TYPE string DEFAULT 'https'
                                          i_major_version TYPE string DEFAULT '1'
                                          i_branch        TYPE string DEFAULT 'release'
                                RETURNING VALUE(r_result) TYPE string,
      build_zipfile_name IMPORTING i_operation     TYPE string
                                   i_version       TYPE string DEFAULT 'LATEST'
                         RETURNING VALUE(r_result) TYPE string,
      build_jsonfile_name IMPORTING i_operation     TYPE string
                                    i_version       TYPE string DEFAULT 'LATEST'
                          RETURNING VALUE(r_result) TYPE string,
      build_zipfile_path IMPORTING i_zipfile_name       TYPE string
                                   i_trans_logical_path TYPE filepath-pathintern
                         RETURNING VALUE(r_result)      TYPE string
                         RAISING   lcx_error,
      validate_zipfile_path IMPORTING i_zipfile_path   TYPE string
                                      i_logsubdir_name TYPE fileintern
                            RETURNING VALUE(r_result)  TYPE abap_bool.

ENDCLASS.

CLASS lcl_sdk_zipfile IMPLEMENTATION.

  METHOD constructor.
    op = i_op.
    version = i_version.

    IF i_name IS INITIAL.
      name = |{ build_zipfile_name( i_operation = op i_version = version ) }|.
    ELSE.
      name = i_name.
    ENDIF.

    IF i_uri IS INITIAL.
      uri = |{ build_download_uri_prefix( ) }{ name }|.
    ELSE.
      uri = i_uri.
    ENDIF.

    IF i_json_web IS INITIAL.
      json_web = |{ build_download_uri_prefix( ) }{ build_jsonfile_name( i_operation = op i_version = version ) }|.
    ELSE.
      json_web = i_json_web.
    ENDIF.

    IF i_path IS INITIAL.
      path = build_zipfile_path( i_zipfile_name = name i_trans_logical_path = lif_sdk_constants=>c_trans_logical_path ).
    ELSE.
      path = i_path.
    ENDIF.

    validate_zipfile_path( i_zipfile_path = path i_logsubdir_name = lif_sdk_constants=>c_logsubdir_name ).

  ENDMETHOD.

  METHOD set_op.
    op = i_op.
  ENDMETHOD.

  METHOD set_version.
    version = i_version.
  ENDMETHOD.

  METHOD set_name.
    name = i_name.
  ENDMETHOD.

  METHOD set_uri.
    uri = i_uri.
  ENDMETHOD.

  METHOD set_path.
    path = i_path.
  ENDMETHOD.

  METHOD set_json_web.
    json_web = i_json_web.
  ENDMETHOD.


  METHOD build_download_uri_prefix.

    r_result = |{ i_protocol }{ lif_sdk_constants=>c_download_uri_prefix }{ i_major_version }/{ i_branch }/| ##NO_TEXT..

  ENDMETHOD.

  METHOD build_zipfile_name.

    CASE i_operation.
      WHEN 'install'.
        r_result = |{ lif_sdk_constants=>c_sdk_inst_file_prefix }{ i_version }.zip| ##NO_TEXT..
      WHEN 'uninstall'.
        r_result = |{ lif_sdk_constants=>c_sdk_uninst_file_prefix }{ i_version }.zip| ##NO_TEXT..
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD build_jsonfile_name.

    CASE i_operation.
      WHEN 'install'.
        r_result = |{ lif_sdk_constants=>c_sdk_inst_file_prefix }{ i_version }.json| ##NO_TEXT..
      WHEN 'uninstall'.
        r_result = |{ lif_sdk_constants=>c_sdk_uninst_file_prefix }{ i_version }.json| ##NO_TEXT..
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.


  METHOD build_zipfile_path.

    CALL FUNCTION 'FILE_GET_NAME_USING_PATH'
      EXPORTING
        client                     = sy-mandt
        logical_path               = i_trans_logical_path
        operating_system           = sy-opsys
        parameter_1                = 'tmp'
        file_name                  = i_zipfile_name
      IMPORTING
        file_name_with_path        = r_result
      EXCEPTIONS
        path_not_found             = 1
        missing_parameter          = 2
        operating_system_not_found = 3
        file_system_not_found      = 4
        OTHERS                     = 5.
    IF sy-subrc <> 0.
      lcl_utils=>raise_lpath_exception( i_zipfile_name ).
    ENDIF.

  ENDMETHOD.


  METHOD validate_zipfile_path.

    DATA(lv_zipfile_path) = i_zipfile_path.

    CALL FUNCTION 'FILE_VALIDATE_NAME'
      EXPORTING
        logical_filename  = i_logsubdir_name
        parameter_1       = 'tmp'
      IMPORTING
        validation_active = r_result
      CHANGING
        physical_filename = lv_zipfile_path
      EXCEPTIONS
        OTHERS            = 1.
    IF sy-subrc <> 0.
      MESSAGE ID sy-msgid TYPE sy-msgty NUMBER sy-msgno
        WITH sy-msgv1 sy-msgv2 sy-msgv3 sy-msgv4.
    ENDIF.

  ENDMETHOD.

ENDCLASS.

CLASS lcl_sdk_zipfile_collection DEFINITION FINAL.

  PUBLIC SECTION.
    TYPES tt_sdk_zipfile TYPE STANDARD TABLE OF REF TO lcl_sdk_zipfile.

    METHODS:
      constructor IMPORTING i_zipfiles         TYPE tt_sdk_zipfile OPTIONAL
                            i_internet_manager TYPE REF TO lif_sdk_internet_manager OPTIONAL
                            i_file_manager     TYPE REF TO lif_sdk_file_manager OPTIONAL
                  RAISING   lcx_error,
      get_by_op_version IMPORTING i_op             TYPE string
                                  i_version        TYPE string
                        RETURNING VALUE(r_zipfile) TYPE REF TO lcl_sdk_zipfile,
      add_pair IMPORTING i_zipfile_inst   TYPE REF TO lcl_sdk_zipfile
                         i_zipfile_uninst TYPE REF TO lcl_sdk_zipfile
               RAISING   lcx_error,
      exists_in_collection_pair IMPORTING i_version       TYPE string
                                RETURNING VALUE(r_result) TYPE abap_bool,
      exists_on_disk_pair IMPORTING i_version       TYPE string
                          RETURNING VALUE(r_result) TYPE abap_bool,
      download_zipfile_pair IMPORTING i_version TYPE string DEFAULT 'LATEST'
                            RAISING   lcx_error,
      ensure_zipfiles_downloaded
        IMPORTING i_version       TYPE string DEFAULT 'LATEST'
        RETURNING VALUE(r_result) TYPE abap_bool
        RAISING   lcx_error.


  PRIVATE SECTION.
    DATA: mt_sdk_zipfiles  TYPE tt_sdk_zipfile,
          internet_manager TYPE REF TO lif_sdk_internet_manager,
          file_manager     TYPE REF TO lif_sdk_file_manager.

    METHODS:
      add IMPORTING i_zipfile TYPE REF TO lcl_sdk_zipfile
          RAISING   lcx_error,
      exists_in_collection IMPORTING i_op            TYPE string
                                     i_version       TYPE string
                           RETURNING VALUE(r_result) TYPE abap_bool,
      exists_on_disk IMPORTING i_operation     TYPE string
                               i_version       TYPE string
                     RETURNING VALUE(r_result) TYPE abap_bool,
      download_zipfile IMPORTING i_zipfile TYPE REF TO lcl_sdk_zipfile
                       RAISING   lcx_error,

      clear_missing_zipfile_paths IMPORTING i_file_manager TYPE REF TO lcl_sdk_file_manager.

ENDCLASS.


CLASS lcl_sdk_zipfile_collection IMPLEMENTATION.


  METHOD constructor.
    IF i_zipfiles IS NOT INITIAL.
      LOOP AT i_zipfiles INTO DATA(wa_zipfile).
        add( wa_zipfile ).
      ENDLOOP.
    ELSE.
      add( NEW lcl_sdk_zipfile( i_op = 'install' i_version  = 'LATEST' ) ).
      add( NEW lcl_sdk_zipfile( i_op = 'uninstall' i_version  = 'LATEST' ) ).
    ENDIF.

    IF i_internet_manager IS BOUND.
      internet_manager = i_internet_manager.
    ELSE.
      internet_manager = NEW lcl_sdk_internet_manager( ).
    ENDIF.

    IF i_file_manager IS BOUND.
      file_manager = i_file_manager.
    ELSE.
      file_manager = NEW lcl_sdk_file_manager( ).
    ENDIF.

  ENDMETHOD.


  METHOD add.
    IF i_zipfile IS NOT BOUND.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Unbound zipfile reference.| ##NO_TEXT.
    ENDIF.
    APPEND i_zipfile TO mt_sdk_zipfiles.
  ENDMETHOD.


  METHOD add_pair.
    IF i_zipfile_inst IS NOT BOUND
    OR i_zipfile_uninst IS NOT BOUND.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Unbound zipfile reference.| ##NO_TEXT.
    ENDIF.
    APPEND i_zipfile_inst TO mt_sdk_zipfiles.
    APPEND i_zipfile_uninst TO mt_sdk_zipfiles.
  ENDMETHOD.


  METHOD exists_in_collection.
    IF get_by_op_version( i_op = i_op i_version = i_version ) IS BOUND.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD exists_in_collection_pair.
    r_result = xsdbool( exists_in_collection( i_op = 'install' i_version = i_version ) = abap_true
        AND exists_in_collection( i_op = 'uninstall' i_version = i_version ) = abap_true ).
  ENDMETHOD.


  METHOD get_by_op_version.
    LOOP AT mt_sdk_zipfiles INTO DATA(wa_zipfile).
      IF wa_zipfile->op = i_op AND wa_zipfile->version = i_version.
        r_zipfile = wa_zipfile.
        RETURN.
      ENDIF.
    ENDLOOP.
    " returns unbound r_zipfile if no zipfile matches op and version
  ENDMETHOD.


  METHOD exists_on_disk.
    IF exists_in_collection( i_op = i_operation i_version = i_version ).
      r_result = file_manager->check_file_exists_at( get_by_op_version( i_op = i_operation i_version = i_version )->path ).
    ELSE.
      r_result = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD exists_on_disk_pair.
    r_result = xsdbool( exists_on_disk( i_operation = 'install' i_version = i_version ) = abap_true
        AND exists_on_disk( i_operation = 'uninstall' i_version = i_version ) = abap_true ).
  ENDMETHOD.



  METHOD download_zipfile_pair.
    DATA l_dir_tmp_path TYPE text255.
    DATA lv_result TYPE abap_bool VALUE abap_true.

    DATA(wa_zipfile_inst) = get_by_op_version( i_op = 'install' i_version = i_version ).
    DATA(wa_zipfile_uninst) = get_by_op_version( i_op = 'uninstall' i_version = i_version ).

    IF wa_zipfile_inst IS NOT BOUND
    AND wa_zipfile_uninst IS NOT BOUND.
      wa_zipfile_inst = NEW lcl_sdk_zipfile( i_op = 'install' i_version = i_version ).
      wa_zipfile_uninst = NEW lcl_sdk_zipfile( i_op = 'uninstall' i_version = i_version ).
    ENDIF.

    download_zipfile( wa_zipfile_inst ).
    download_zipfile( wa_zipfile_uninst ).

  ENDMETHOD.


  METHOD download_zipfile.
    IF file_manager->check_file_writable_at( i_path = i_zipfile->path ) = abap_false.
      MESSAGE |File at path { i_zipfile->path } not writable!| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      EXIT.
    ENDIF.

    cl_progress_indicator=>progress_indicate( i_text               = |Downloading { i_zipfile->name } ...|
                                              i_processed          = sy-tabix
                                              i_output_immediately = abap_true ) ##NO_TEXT.

    DATA(e_zipfile_blob) = internet_manager->download( i_absolute_uri = i_zipfile->uri
                                                       i_blankstocrlf = abap_false ).

    TRY.
        file_manager->open_for_output( i_zipfile->path ).
        TRANSFER e_zipfile_blob TO i_zipfile->path.
        CLOSE DATASET i_zipfile->path.
      CATCH cx_sy_file_authority INTO DATA(r_ex1).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = r_ex1
            iv_msg   = r_ex1->get_text( ).
      CATCH cx_sy_file_access_error INTO DATA(r_ex2).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = r_ex2
            iv_msg   = r_ex1->get_text( ).
      CATCH cx_root INTO DATA(r_ex).
        RAISE EXCEPTION TYPE lcx_error
          EXPORTING
            previous = r_ex
            iv_msg   = r_ex1->get_text( ).
    ENDTRY.

    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not convert data to XSTRING.| ##NO_TEXT.
    ENDIF.


    IF NOT exists_in_collection( i_op = i_zipfile->op i_version = i_zipfile->version ).
      add( i_zipfile ).
    ENDIF.

  ENDMETHOD.

  METHOD ensure_zipfiles_downloaded.

    DATA lv_result TYPE abap_bool.
    lv_result = abap_true.

    IF NOT exists_on_disk_pair( i_version = i_version ).

      MESSAGE |ABAP SDK zipfile for installation/uninstallation in version { i_version } not present. Downloading them now.| TYPE 'I' ##NO_TEXT.

      TRY.
          download_zipfile_pair( i_version = i_version ).
        CATCH lcx_error.
          MESSAGE 'One or more ABAP SDK zipfiles could not be successfully downloaded, aborting' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      ENDTRY.

    ENDIF.

    r_result = lv_result.

  ENDMETHOD.




  " TODO: Currently not being used. Kept for future implementation of a mgmt dialog for zipfiles on disk
  METHOD clear_missing_zipfile_paths.

    DATA(zipfile_available) = abap_false.

    LOOP AT mt_sdk_zipfiles INTO DATA(wa_zipfile).

      zipfile_available = i_file_manager->lif_sdk_file_manager~check_file_exists_at( i_path = wa_zipfile->path ).

      IF zipfile_available = abap_false.
        wa_zipfile->set_path( i_path = '' ).
      ENDIF.

    ENDLOOP.
  ENDMETHOD.

ENDCLASS.





CLASS lcl_sdk_package_manager DEFINITION FINAL.

  PUBLIC SECTION.

    DATA:
      sdk_zipfiles                TYPE REF TO lcl_sdk_zipfile_collection READ-ONLY,
      mt_available_modules_inst   TYPE tt_sdk_module,   " available modules for installation  (i.e. install transports)
      mt_available_modules_uninst TYPE tt_sdk_module.   " available modules for uninstallation (i.e. uninstall transports)

    DATA: internet_manager TYPE REF TO lif_sdk_internet_manager READ-ONLY.
    DATA: certificate_manager TYPE REF TO lif_sdk_certificate_manager READ-ONLY.
    DATA: file_manager TYPE REF TO lif_sdk_file_manager  READ-ONLY.
    DATA: transport_manager TYPE REF TO lif_sdk_transport_manager  READ-ONLY.
    DATA: job_manager TYPE REF TO lif_sdk_job_manager  READ-ONLY.


    METHODS:
      constructor IMPORTING i_batch_mode          TYPE syst-batch OPTIONAL
                            i_internet_manager    TYPE REF TO lif_sdk_internet_manager OPTIONAL
                            i_certificate_manager TYPE REF TO lif_sdk_certificate_manager OPTIONAL
                            i_file_manager        TYPE REF TO lcl_sdk_file_manager OPTIONAL
                            i_transport_manager   TYPE REF TO lif_sdk_transport_manager OPTIONAL
                            i_job_manager         TYPE REF TO lif_sdk_job_manager OPTIONAL
                            i_sdk_zipfiles        TYPE REF TO lcl_sdk_zipfile_collection OPTIONAL
                  RAISING   lcx_error,
      install_all_modules IMPORTING it_modules_to_be_installed TYPE tt_sdk_tla
                                    it_modules_to_be_deleted   TYPE tt_sdk_tla
                          RETURNING VALUE(r_jobnumber)         TYPE btcjobcnt
                          RAISING   lcx_error,
      get_sdk_installed_modules RETURNING VALUE(r_installed_modules) TYPE tt_sdk_module
                                RAISING   lcx_error,
      get_sdk_deprecated_mod_inst RETURNING VALUE(r_deprecated_modules) TYPE tt_sdk_module,
      get_sdk_avail_modules_json IMPORTING i_operation                TYPE string
                                           i_source                   TYPE string
                                           i_version                  TYPE string DEFAULT 'LATEST'
                                 RETURNING VALUE(r_available_modules) TYPE tt_sdk_module
                                 RAISING   lcx_error,
      update_zipfiles_if_outdated IMPORTING i_avers_core_inst   TYPE string
                                            i_avers_core_uninst TYPE string
                                  RETURNING VALUE(r_result)     TYPE abap_bool
                                  RAISING   lcx_error,
      run_foreground IMPORTING i_modules_to_be_installed TYPE tt_sdk_tla
                               i_modules_to_be_deleted   TYPE tt_sdk_tla
                               i_target_version          TYPE string
                     EXPORTING e_tp_rc_inst              TYPE stpa-retcode
                               es_exception_inst         TYPE stmscalert
                               e_tp_rc_uninst            TYPE stpa-retcode
                               es_exception_uninst       TYPE stmscalert
                     RAISING   lcx_error,
      run_background RAISING   lcx_error.


  PROTECTED SECTION.

  PRIVATE SECTION.


ENDCLASS.



CLASS lcl_sdk_package_manager IMPLEMENTATION.

  METHOD constructor.

    IF lcl_utils=>has_prod_client( ).
      MESSAGE |System has at least one productive client, please use TMS.| TYPE 'A' ##NO_TEXT.
    ENDIF.

    IF i_internet_manager IS BOUND.
      internet_manager = i_internet_manager.
    ELSE.
      internet_manager = NEW lcl_sdk_internet_manager( ).
    ENDIF.

    IF i_certificate_manager IS BOUND.
      certificate_manager = i_certificate_manager.
    ELSE.
      certificate_manager = NEW lcl_sdk_certificate_manager( i_internet_manager = internet_manager ).
    ENDIF.

    IF i_file_manager IS BOUND.
      file_manager = i_file_manager.
    ELSE.
      file_manager = file_manager = NEW lcl_sdk_file_manager( ).
    ENDIF.

    IF i_transport_manager IS BOUND.
      transport_manager = i_transport_manager.
    ELSE.
      transport_manager = NEW lcl_sdk_transport_manager( ).
    ENDIF.

    IF i_job_manager IS BOUND.
      job_manager = i_job_manager.
    ELSE.
      job_manager = NEW lcl_sdk_job_manager( ).
    ENDIF.

    IF i_sdk_zipfiles IS BOUND.
      sdk_zipfiles = i_sdk_zipfiles.
    ELSE.
      sdk_zipfiles = NEW lcl_sdk_zipfile_collection( ).
    ENDIF.

    TRY.
        IF internet_manager->has_internet_access( ) = abap_false.
          MESSAGE |This report requires HTTP(S) Internet access, please enable before usage.| TYPE 'A' ##NO_TEXT.
        ENDIF.
      CATCH lcx_error INTO DATA(r_error).
        MESSAGE |This report requires HTTP(S) Internet access, please enable before usage.| TYPE 'A' ##NO_TEXT.
    ENDTRY.

    mt_available_modules_inst = get_sdk_avail_modules_json( i_operation = 'install'
                                                            i_source    = 'web'
                                                            i_version   = 'LATEST' ) ##NO_TEXT.

    mt_available_modules_uninst = get_sdk_avail_modules_json( i_operation = 'uninstall'
                                                              i_source    = 'web'
                                                              i_version   = 'LATEST' ) ##NO_TEXT.

    IF i_batch_mode = abap_true.
      run_background( ).
    ENDIF.

  ENDMETHOD.

  METHOD install_all_modules.

    DATA: lt_modules_to_be_installed TYPE tt_sdk_tla.
    DATA: lt_modules_to_be_deleted TYPE tt_sdk_tla.
    DATA: lv_target_version TYPE string VALUE 'LATEST'.

    DATA(lt_available_modules_cv) = get_sdk_avail_modules_json( i_operation = 'install'
                                                                i_source    = 'web'
                                                                i_version   = lv_target_version ).

    DATA: wa_available_module_cv TYPE ts_sdk_module.
    LOOP AT lt_available_modules_cv INTO wa_available_module_cv.
      INSERT VALUE ts_sdk_tla( tla = wa_available_module_cv-tla version = lv_target_version ) INTO TABLE lt_modules_to_be_installed .
    ENDLOOP.


    IF lines( lt_modules_to_be_installed ) > 0.

      IF sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
        RETURN.
      ENDIF.

      IF update_zipfiles_if_outdated( i_avers_core_inst = mt_available_modules_inst[ tla = 'core' ]-avers
                                      i_avers_core_uninst = mt_available_modules_uninst[ tla = 'core' ]-avers ) = abap_false.
        RETURN.
      ENDIF.

    ENDIF.

    r_jobnumber = job_manager->submit_batch_job( i_modules_to_be_installed = lt_modules_to_be_installed
                                                 i_modules_to_be_deleted   = lt_modules_to_be_deleted
                                                 i_target_version          = lv_target_version ).

  ENDMETHOD.



  METHOD update_zipfiles_if_outdated.

    DATA lv_result TYPE abap_bool.
    DATA lv_dl_result TYPE abap_bool.
    r_result = abap_true.

    DATA(lt_mod_avail_inst_zip) = get_sdk_avail_modules_json( i_operation = 'install'
                                                              i_source    = 'zip'
                                                              i_version   = 'LATEST' ).
    DATA(lt_mod_avail_uninst_zip) = get_sdk_avail_modules_json( i_operation = 'uninstall'
                                                                i_source    = 'zip'
                                                                i_version   = 'LATEST' ).

    IF lcl_utils=>cmp_version_string( i_string1 = i_avers_core_inst
                                      i_string2 = lt_mod_avail_inst_zip[ tla = 'core' ]-avers ) = 1
        OR lcl_utils=>cmp_version_string( i_string1 = i_avers_core_uninst
                                          i_string2 = lt_mod_avail_uninst_zip[ tla = 'core' ]-avers ) = 1.
      MESSAGE 'One or more ABAP SDK zipfiles not current. Downloading new version now.' TYPE 'I' ##NO_TEXT.
      TRY.
          sdk_zipfiles->download_zipfile_pair( i_version = 'LATEST' ).
        CATCH lcx_error.
          MESSAGE 'One or more ABAP SDK zipfiles could not be successfully downloaded, aborting' TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
          r_result = abap_false.
      ENDTRY.
    ENDIF.

  ENDMETHOD.



  METHOD get_sdk_installed_modules.

    DATA lt_installed_modules_list TYPE tt_sdk_module.
    DATA ls_installed_module TYPE ts_sdk_module.

    DATA l_abap_matcher TYPE REF TO cl_abap_matcher.
    DATA l_abap_regex_posix TYPE REF TO cl_abap_regex.

    SELECT devclass, ctext FROM tdevct
        WHERE spras = 'E'
        AND devclass LIKE '/AWS1/API_%'
        INTO (@DATA(l_sdk_devclass), @DATA(l_sdk_ctext)). "#EC CI_SGLSELECT
      IF strlen( l_sdk_devclass ) <= 13.
        ls_installed_module-tla = to_lower( substring_after( val = l_sdk_devclass
                                                             sub = '/AWS1/API_' ) ).
        ls_installed_module-name = l_sdk_ctext.
        APPEND ls_installed_module TO lt_installed_modules_list.
      ENDIF.
    ENDSELECT.

    IF sy-subrc <> 0.
      " no issue, no SDK modules are installed yet
      CLEAR lt_installed_modules_list.
    ENDIF.

    DATA: l_collect_date TYPE sy-datum,
          l_collect_time TYPE sy-uzeit,
          l_collect_flag TYPE stms_flag,
          ls_exception   TYPE stmscalert.


    DATA: lt_buffer  TYPE STANDARD TABLE OF tmsbuffer,
          lt_counter TYPE STANDARD TABLE OF tmsbufcnt,
          lt_project TYPE STANDARD TABLE OF tmsbufpro,
          lt_domain  TYPE STANDARD TABLE OF tmscdom,
          lt_system  TYPE STANDARD TABLE OF tmscsys,
          lt_group   TYPE STANDARD TABLE OF tmscnfs.

    CALL FUNCTION 'TMS_MGR_READ_TRANSPORT_QUEUE'
      IMPORTING
        ev_collect_date    = l_collect_date
        ev_collect_time    = l_collect_time
        ev_collect_flag    = l_collect_flag
        es_exception       = ls_exception
      TABLES
        tt_buffer          = lt_buffer
        tt_counter         = lt_counter
        tt_project         = lt_project
        tt_domain          = lt_domain
        tt_system          = lt_system
        tt_group           = lt_group
      EXCEPTIONS
        read_config_failed = 1
        OTHERS             = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not read TMS queue| ##NO_TEXT.
    ENDIF.

    CLEAR ls_exception.


    DATA l_system TYPE tmssysnam.


    DATA: l_service_vers  TYPE tmsrel,
          lt_tmstpalogs   TYPE tmstpalogs,
          lt_irt_requests TYPE STANDARD TABLE OF ctrs_requ.


    l_system = lcl_utils=>get_system_name( ).

    CALL FUNCTION 'TMS_TM_GET_TRLIST'
      EXPORTING
        iv_system       = l_system
        iv_startdate    = '19000101'
        iv_starttime    = '000000'
        iv_enddate      = sy-datum
        iv_endtime      = sy-uzeit
        iv_allcli       = ''
        iv_imports      = 'X'
      IMPORTING
        ev_service_vers = l_service_vers
        et_tmstpalog    = lt_tmstpalogs
        es_exception    = ls_exception
      TABLES
        irt_requests    = lt_irt_requests
      EXCEPTIONS
        alert           = 1
        OTHERS          = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not read get TR list from { l_system }| ##NO_TEXT.
    ENDIF.


    DATA ls_tmstpalog TYPE tmstpalog.


    LOOP AT lt_installed_modules_list ASSIGNING FIELD-SYMBOL(<fs_installed_module>).

      LOOP AT lt_tmstpalogs ASSIGNING FIELD-SYMBOL(<fs_tmstpalog_entry>).

        DATA(l_truser) = 'AWS_' && <fs_installed_module>-tla.

        IF <fs_installed_module>-tla = 'sts' OR
            <fs_installed_module>-tla = 's3' OR
            <fs_installed_module>-tla = 'smr' OR
            <fs_installed_module>-tla = 'rla'.
          l_truser = 'AWS_CORE'.
          IF lines( mt_available_modules_inst ) > 0.
            <fs_installed_module>-avers = me->mt_available_modules_inst[ tla = 'core' ]-avers.
          ENDIF.
        ENDIF.

        " Find the transport log line pertaining to the tla of the installed module under scrutiny
        IF <fs_tmstpalog_entry>-truser = to_upper( l_truser ). "AND ( <fs_tmstpalog_entry>-retcode = '0000' OR <fs_tmstpalog_entry>-retcode = '0004' ).

          " If the transport log line has no timestamp, assume it's our latest transport
          IF ls_tmstpalog-trtime IS INITIAL.
            ls_tmstpalog = <fs_tmstpalog_entry>.
          ENDIF.

          " If we find a more recent transport while cycling through the transport log, this becomes our candidate for figuring out the current version
          IF <fs_tmstpalog_entry>-trtime >= ls_tmstpalog-trtime.
            ls_tmstpalog = <fs_tmstpalog_entry>.
          ENDIF.

        ENDIF.



      ENDLOOP.
      DATA(lo_regex) = NEW cl_abap_regex( pattern = '\d{1,}\.\d{1,}\.\d{0,}' ) ##REGEX_POSIX.
      l_abap_matcher = lo_regex->create_matcher( text = ls_tmstpalog-as4text ).

      DATA(lt_results) = l_abap_matcher->find_all( ).

      READ TABLE lt_results INTO DATA(wa_result) INDEX 1.
      IF sy-subrc <> 0.
        MESSAGE |Expected to find exactly one version number in text { ls_tmstpalog-as4text }|
           TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
        RETURN.
      ENDIF.

      DATA l_cvers_string TYPE string.

      l_cvers_string = substring( val = ls_tmstpalog-as4text
                                  off = wa_result-offset
                                  len = wa_result-length ).

      <fs_installed_module>-cvers = l_cvers_string.

      " if not already set as part of core
      IF <fs_installed_module>-avers IS INITIAL AND lines( mt_available_modules_inst ) > 0.
        <fs_installed_module>-avers = VALUE #( me->mt_available_modules_inst[ tla = <fs_installed_module>-tla ]-avers DEFAULT 'n.a.' ).
      ENDIF.

      <fs_installed_module>-ctransport = ls_tmstpalog-trkorr.
      <fs_installed_module>-tp_rc = ls_tmstpalog-retcode.

      CASE <fs_installed_module>-tp_rc.
        WHEN '0000'.
          <fs_installed_module>-tp_icon = '@5B@'.
          <fs_installed_module>-tp_text = 'Imported successfully.'.
        WHEN '0004'.
          <fs_installed_module>-tp_icon = '@5D@'.
          <fs_installed_module>-tp_text = 'Imported with warnings.'.
        WHEN '0008'.
          <fs_installed_module>-tp_icon = '@5C@'.
          <fs_installed_module>-tp_text = 'Imported with errors.'.
        WHEN 'OTHERS'.
          <fs_installed_module>-tp_icon = '@AG@'.
          <fs_installed_module>-tp_text = 'Unknown error, check transport logs.'.
      ENDCASE.



      CLEAR ls_tmstpalog.

    ENDLOOP.

    r_installed_modules = lt_installed_modules_list.

  ENDMETHOD.



  METHOD get_sdk_deprecated_mod_inst.

    TRY.
        DATA(lt_installed_modules) = get_sdk_installed_modules( ).
      CATCH lcx_error.
    ENDTRY.

    LOOP AT lt_installed_modules INTO DATA(wa_inst).
      IF wa_inst-tla = 's3' OR
         wa_inst-tla = 'rla' OR
         wa_inst-tla = 'sts' OR
         wa_inst-tla = 'smr'.
        CONTINUE.
      ELSE.
        READ TABLE mt_available_modules_inst WITH KEY tla = wa_inst-tla TRANSPORTING NO FIELDS.
        IF sy-subrc <> 0.
          APPEND VALUE #( tla = wa_inst-tla ) TO r_deprecated_modules.
        ENDIF.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.




  METHOD get_sdk_avail_modules_json.

    DATA l_sdk_json TYPE w3_url.
    DATA lts_sdk_avail_modules TYPE tt_sdk_module.

    CASE i_source.
      WHEN 'web'.

        DATA(l_jsonx) = internet_manager->download( i_absolute_uri = sdk_zipfiles->get_by_op_version( i_op = i_operation i_version = i_version )->json_web
                                                    i_blankstocrlf = abap_false ).


      WHEN 'zip'.

        l_jsonx = file_manager->get_file_from_zip( i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = i_operation i_version = i_version )->path
                                                   i_file_to_retrieve      = lif_sdk_constants=>c_sdk_index_json_zip_path ).

      WHEN 'others'.

    ENDCASE.


    DATA(l_reader) = cl_sxml_string_reader=>create( input       = l_jsonx
                                                    normalizing = abap_true ).


    DATA: BEGIN OF wa_node,
            node_type TYPE string,
            name      TYPE string,
            value     TYPE string,
            depth     TYPE i VALUE 0,
          END OF wa_node,
          lt_transport_nodes LIKE TABLE OF wa_node.

    DATA depth_counter TYPE i VALUE 0.

    DO.
      CLEAR wa_node.
      DATA(l_node) = l_reader->read_next_node( ).
      IF l_node IS INITIAL.
        EXIT.
      ENDIF.
      CASE l_node->type.
        WHEN if_sxml_node=>co_nt_element_open.
          depth_counter = depth_counter + 1.
          DATA(l_open_element) = CAST if_sxml_open_element( l_node ).
          DATA(lt_attributes) = l_open_element->get_attributes( ).
          LOOP AT lt_attributes INTO DATA(l_attribute_node).
            wa_node-node_type = 'attribute'.
            wa_node-name = l_attribute_node->qname-name.
            IF l_attribute_node->value_type = if_sxml_value=>co_vt_text.
              wa_node-value = l_attribute_node->get_value( ).
            ENDIF.
            wa_node-depth = depth_counter.
            APPEND wa_node TO lt_transport_nodes.
          ENDLOOP.
          CONTINUE.
        WHEN if_sxml_node=>co_nt_element_close.
          depth_counter = depth_counter - 1.
          CONTINUE.
        WHEN if_sxml_node=>co_nt_value.
          DATA(l_value_node) = CAST if_sxml_value_node( l_node ).
          wa_node-node_type = 'value'.
          IF l_value_node->value_type = if_sxml_value=>co_vt_text.
            wa_node-value = l_value_node->get_value( ).
          ENDIF.
          wa_node-depth = depth_counter.
          APPEND wa_node TO lt_transport_nodes.
          CONTINUE.
      ENDCASE.

    ENDDO.

    DATA wa_module TYPE ts_sdk_module.
    DATA(i) = 1.
    DATA(j) = 0.  " Offset for TLA -> name
    DATA(k) = 0.  " Offset for Groups -> popular -> title -> tla

    LOOP AT lt_transport_nodes ASSIGNING FIELD-SYMBOL(<ls_transport_node>).

      CLEAR wa_module.
      IF <ls_transport_node>-value = 'core'.
        wa_module-tla = 'core'. " tla nametag
        wa_module-atransport = lt_transport_nodes[ 8 ]-value. " transport name
        wa_module-avers = lt_transport_nodes[ 4 ]-value. " version
        wa_module-is_core = abap_true. " is core
        INSERT wa_module INTO TABLE lts_sdk_avail_modules.
        CONTINUE.
      ENDIF.

      IF i >= 16 AND j < i AND i < lines( lt_transport_nodes ) AND lt_transport_nodes[ i ]-depth > 2.

        wa_module-tla = lt_transport_nodes[ i ]-value. " tla nametag
        wa_module-atransport = lt_transport_nodes[ i + 2 ]-value. " transport name
        wa_module-is_core = lt_transport_nodes[ i + 6 ]-value. " is core
        wa_module-avers = lt_transport_nodes[ 4 ]-value. " version
        INSERT wa_module INTO TABLE lts_sdk_avail_modules.
        i = i + 7.
      ELSEIF i < 16.
        i = i + 1.
        CONTINUE.
      ELSE.
        j = i.
        EXIT.
      ENDIF.

    ENDLOOP.


    LOOP AT lts_sdk_avail_modules INTO wa_module.

      IF wa_module-tla = 'core'.
        wa_module-name = 'AWS SDK for SAP ABAP core [s3, smr, rla, sts]' ##NO_TEXT.
      ENDIF.

      DATA(j_offset) = 0.
      WHILE j_offset < lines( lt_transport_nodes ) - j.
        IF lt_transport_nodes[ j + j_offset ]-value = wa_module-tla.
          wa_module-name = lt_transport_nodes[ j_offset + j + 2 ]-value.
          EXIT.
        ENDIF.
        j_offset = j_offset + 1.
      ENDWHILE.

      MODIFY lts_sdk_avail_modules FROM wa_module TRANSPORTING tla name.

    ENDLOOP.


    k = j + j_offset + 8.
    DATA(k_offset) = 0.

    WHILE k_offset <= lines( lt_transport_nodes ) - k.
      IF line_exists( lts_sdk_avail_modules[ tla = lt_transport_nodes[ k + k_offset ]-value ] ).
        lts_sdk_avail_modules[ tla = lt_transport_nodes[ k + k_offset ]-value ]-is_popular = abap_true .
      ENDIF.
      k_offset = k_offset + 1.
    ENDWHILE.



    r_available_modules = lts_sdk_avail_modules.

  ENDMETHOD.


  METHOD run_foreground.

    DATA: l_tp_ret_code_inst TYPE stpa-retcode.
    DATA: ls_exception_inst TYPE stmscalert.

    DATA: l_tp_ret_code_uninst TYPE stpa-retcode.
    DATA: ls_exception_uninst TYPE stmscalert.

    DATA(lt_modules_to_be_installed) = i_modules_to_be_installed.
    DATA(lt_modules_to_be_deleted) = i_modules_to_be_deleted.

    DATA lt_transport_list_inst TYPE stms_tr_requests.
    DATA lt_transport_list_uninst TYPE stms_tr_requests.

    DATA lt_avail_modules_inst TYPE tt_sdk_module.
    lt_avail_modules_inst = get_sdk_avail_modules_json(
      i_operation = 'install'
      i_source    = 'zip'
      i_version   = i_target_version
    ).

    LOOP AT lt_modules_to_be_installed ASSIGNING FIELD-SYMBOL(<fs_module_to_be_installed>).

      transport_manager->get_sdk_cofile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla
          i_transport             = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport
          i_operation             = 'install'
          i_version               = i_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'install' i_version = i_target_version )->path
        IMPORTING
          e_cofile_name           = DATA(l_cofile_name)
          e_cofile_blob           = DATA(l_cofile_blob) ).


      transport_manager->get_sdk_datafile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla
          i_transport             = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport
          i_operation             = 'install'
          i_version               = i_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'install' i_version = i_target_version )->path
        IMPORTING
          e_datafile_name         = DATA(l_datafile_name)
          e_datafile_blob         = DATA(l_datafile_blob) ).


      DATA(l_success_write) = transport_manager->write_sdk_transport_trdir(
        i_cofile_name   = l_cofile_name
        i_datafile_name = l_datafile_name
        i_cofile_blob   = l_cofile_blob
        i_datafile_blob = l_datafile_blob ).

      INSERT VALUE stms_tr_request(
      trkorr = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport
      tarcli = sy-mandt
      ) INTO TABLE lt_transport_list_inst.

    ENDLOOP.

    CALL METHOD transport_manager->import_sdk_transports
      EXPORTING
        it_transport_names = lt_transport_list_inst
      IMPORTING
        e_tp_retcode       = l_tp_ret_code_inst
        es_exception       = ls_exception_inst.

    e_tp_rc_inst = l_tp_ret_code_inst.
    es_exception_inst = ls_exception_inst.





    LOOP AT lt_modules_to_be_deleted ASSIGNING FIELD-SYMBOL(<fs_module_to_be_deleted>).

      DATA lt_avail_modules_uninst TYPE tt_sdk_module.
      lt_avail_modules_uninst = get_sdk_avail_modules_json(
        i_operation = 'uninstall'
        i_source    = 'zip'
        i_version   = i_target_version
      ).


      transport_manager->get_sdk_cofile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-tla
          i_transport             = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-atransport
          i_operation             = 'uninstall'
          i_version               = i_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'uninstall' i_version = i_target_version )->path
        IMPORTING
          e_cofile_name           = DATA(l_uninstall_cofile_name)
          e_cofile_blob           = DATA(l_uninstall_cofile_blob) ).


      transport_manager->get_sdk_datafile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-tla
          i_transport             = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-atransport
          i_operation             = 'uninstall'
          i_version               = i_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'uninstall' i_version = i_target_version )->path
        IMPORTING
          e_datafile_name         = DATA(l_uninstall_datafile_name)
          e_datafile_blob         = DATA(l_uninstall_datafile_blob) ).


      transport_manager->write_sdk_transport_trdir(
        EXPORTING
          i_cofile_name   = l_uninstall_cofile_name
          i_datafile_name = l_uninstall_datafile_name
          i_cofile_blob   = l_uninstall_cofile_blob
          i_datafile_blob = l_uninstall_datafile_blob
        RECEIVING
          r_success       = DATA(l_uninstall_success_write) ).


      INSERT VALUE stms_tr_request(
      trkorr = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-atransport
      tarcli = sy-mandt
      ) INTO TABLE lt_transport_list_uninst.

    ENDLOOP.

    CALL METHOD transport_manager->import_sdk_transports
      EXPORTING
        it_transport_names = lt_transport_list_uninst
      IMPORTING
        e_tp_retcode       = l_tp_ret_code_uninst
        es_exception       = ls_exception_uninst.

    e_tp_rc_uninst = l_tp_ret_code_uninst.
    es_exception_uninst = ls_exception_uninst.


  ENDMETHOD.





  METHOD run_background.

    DATA: l_tp_rc_inst TYPE stpa-retcode.
    DATA: ls_exception_inst TYPE stmscalert.
    DATA: l_tp_rc_uninst TYPE stpa-retcode.
    DATA: ls_exception_uninst TYPE stmscalert.


    DATA: st_modules_to_be_installed TYPE tt_sdk_tla.
    DATA: st_modules_to_be_deleted TYPE tt_sdk_tla.
    DATA: s_target_version TYPE string.

    DATA: lt_transport_list_inst TYPE stms_tr_requests.
    DATA: lt_transport_list_uninst TYPE stms_tr_requests.

    IMPORT st_modules_to_be_installed = st_modules_to_be_installed FROM SHARED BUFFER indx(mi) ID 'MOD_INST'.
    IMPORT st_modules_to_be_deleted = st_modules_to_be_deleted FROM SHARED BUFFER indx(md) ID 'MOD_DELE'.
    IMPORT s_target_version = s_target_version FROM SHARED BUFFER indx(tv) ID 'TAR_VERS'.


    IF sdk_zipfiles->exists_on_disk_pair( i_version = s_target_version ) = abap_false.

      DATA(wa_zipfile_inst) = NEW lcl_sdk_zipfile( i_op = 'install' i_version = s_target_version ).
      DATA(wa_zipfile_uninst) = NEW lcl_sdk_zipfile( i_op = 'uninstall' i_version = s_target_version ).
      sdk_zipfiles->add_pair( i_zipfile_inst   = wa_zipfile_inst
                              i_zipfile_uninst = wa_zipfile_uninst ).
      sdk_zipfiles->download_zipfile_pair( i_version = s_target_version ).

    ELSE.
      wa_zipfile_inst = sdk_zipfiles->get_by_op_version( i_op = 'install' i_version = s_target_version ).
      wa_zipfile_uninst = sdk_zipfiles->get_by_op_version( i_op = 'uninstall' i_version = s_target_version ).
    ENDIF.



    WRITE / |In background mode| ##NO_TEXT.
    WRITE /.

    WRITE /.
    WRITE / |Modules to be installed (transport file { wa_zipfile_inst->path }| ##NO_TEXT.
    LOOP AT st_modules_to_be_installed ASSIGNING FIELD-SYMBOL(<fs_mod_inst>).
      WRITE <fs_mod_inst>-tla && | |.
    ENDLOOP.

    WRITE /.
    WRITE / |to be deleted (transport file { wa_zipfile_uninst->path }| ##NO_TEXT.
    LOOP AT st_modules_to_be_deleted ASSIGNING FIELD-SYMBOL(<fs_mod_dele>).
      WRITE / <fs_mod_dele>-tla && | |.
    ENDLOOP.

    DATA lt_avail_modules_inst TYPE tt_sdk_module.
    lt_avail_modules_inst = get_sdk_avail_modules_json(
      i_operation = wa_zipfile_inst->op
      i_source    = 'zip'
      i_version   = wa_zipfile_inst->version
    ).

    LOOP AT st_modules_to_be_installed ASSIGNING FIELD-SYMBOL(<fs_module_to_be_installed>).
      transport_manager->get_sdk_cofile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla
          i_transport             = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport
          i_operation             = 'install'
          i_version               = s_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'install' i_version = s_target_version )->path
        IMPORTING
          e_cofile_name           = DATA(l_cofile_name)
          e_cofile_blob           = DATA(l_cofile_blob) ).

      WRITE / |Downloaded cofile | && lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport &&
               | for TLA | && lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla ##NO_TEXT.

      transport_manager->get_sdk_datafile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla
          i_transport             = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport
          i_operation             = 'install'
          i_version               = s_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'install' i_version = s_target_version )->path
        IMPORTING
          e_datafile_name         = DATA(l_datafile_name)
          e_datafile_blob         = DATA(l_datafile_blob) ).

      WRITE / |Downloaded data file | && lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport &&
               | for TLA | && lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla ##NO_TEXT.

      transport_manager->write_sdk_transport_trdir(
        EXPORTING
          i_cofile_name   = l_cofile_name
          i_datafile_name = l_datafile_name
          i_cofile_blob   = l_cofile_blob
          i_datafile_blob = l_datafile_blob
        RECEIVING
          r_success       = DATA(l_success_write) ).

      WRITE / |Wrote cofile and datafile for transport | && lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport &&
               | for TLA | && lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-tla && | successfully? -> | && l_success_write ##NO_TEXT.


      INSERT VALUE stms_tr_request( trkorr = lt_avail_modules_inst[ tla = <fs_module_to_be_installed>-tla ]-atransport ) INTO TABLE lt_transport_list_inst.
    ENDLOOP.


    CALL METHOD transport_manager->import_sdk_transports
      EXPORTING
        it_transport_names = lt_transport_list_inst
      IMPORTING
        e_tp_retcode       = l_tp_rc_inst
        es_exception       = ls_exception_inst.

    WRITE / |Imported installation transport with tp return code: | && l_tp_rc_inst && |, message was: | && ls_exception_inst-error ##NO_TEXT.


    DATA lt_avail_modules_uninst TYPE tt_sdk_module.
    lt_avail_modules_uninst = get_sdk_avail_modules_json(
      i_operation = wa_zipfile_uninst->op
      i_source    = 'zip'
      i_version   = wa_zipfile_uninst->version
    ).


    LOOP AT st_modules_to_be_deleted ASSIGNING FIELD-SYMBOL(<fs_module_to_be_deleted>).


      transport_manager->get_sdk_cofile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-tla
          i_transport             = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-atransport
          i_operation             = 'uninstall'
          i_version               = s_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'uninstall' i_version = s_target_version )->path
        IMPORTING
          e_cofile_name           = DATA(l_uninstall_cofile_name)
          e_cofile_blob           = DATA(l_uninstall_cofile_blob) ).


      transport_manager->get_sdk_datafile_from_zip(
        EXPORTING
          i_tla                   = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-tla
          i_transport             = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-atransport
          i_operation             = 'uninstall'
          i_version               = s_target_version
          i_zipfile_absolute_path = sdk_zipfiles->get_by_op_version( i_op = 'uninstall' i_version = s_target_version )->path
        IMPORTING
          e_datafile_name         = DATA(l_uninstall_datafile_name)
          e_datafile_blob         = DATA(l_uninstall_datafile_blob) ).


      transport_manager->write_sdk_transport_trdir(
        EXPORTING
          i_cofile_name   = l_uninstall_cofile_name
          i_datafile_name = l_uninstall_datafile_name
          i_cofile_blob   = l_uninstall_cofile_blob
          i_datafile_blob = l_uninstall_datafile_blob
        RECEIVING
          r_success       = DATA(l_uninstall_success_write) ).

      INSERT VALUE stms_tr_request( trkorr = lt_avail_modules_uninst[ tla = <fs_module_to_be_deleted>-tla ]-atransport ) INTO TABLE lt_transport_list_uninst.

    ENDLOOP.


    CALL METHOD transport_manager->import_sdk_transports
      EXPORTING
        it_transport_names = lt_transport_list_uninst
      IMPORTING
        e_tp_retcode       = l_tp_rc_uninst
        es_exception       = ls_exception_uninst.

    WRITE / |Imported uninstallation transports with tp return code: | && l_tp_rc_uninst && |, message was: | && ls_exception_uninst-error ##NO_TEXT.

    DELETE FROM SHARED BUFFER indx(mi) ID 'MOD_INST'.
    DELETE FROM SHARED BUFFER indx(md) ID 'MOD_DELE'.
    DELETE FROM SHARED BUFFER indx(tv) ID 'TAR_VERS'.

  ENDMETHOD.


ENDCLASS.



CLASS lcl_ui_tree_controller DEFINITION FINAL.
  PUBLIC SECTION.

    INTERFACES
      if_salv_csqt_content_manager.

    TYPES:
      BEGIN OF ts_named_salv_node_key,
        node_name TYPE string,
        key       TYPE salv_de_node_key,
      END OF ts_named_salv_node_key,
      tt_named_salv_node_key TYPE STANDARD TABLE OF ts_named_salv_node_key WITH KEY node_name.


    DATA:
      mr_sdk_package_manager       TYPE REF TO lcl_sdk_package_manager,
      mr_container                 TYPE REF TO cl_gui_container,
      mr_tree                      TYPE REF TO cl_salv_tree,
      mt_treetab                   TYPE tt_sdk_module,
      mt_installed_modules         TYPE tt_sdk_module,
      mt_available_modules_inst    TYPE tt_sdk_module,
      mt_available_modules_uninst  TYPE tt_sdk_module,
      mt_popular_modules           TYPE tt_sdk_tla,
      mt_modules_to_be_installed   TYPE tt_sdk_tla,
      mt_modules_to_be_deleted     TYPE tt_sdk_tla,
      mt_modules_to_be_updated     TYPE tt_sdk_tla,
      m_sdk_available_version      TYPE string,
      mt_folder_node_keys          TYPE tt_named_salv_node_key,
      m_sdk_core_expanded          TYPE abap_bool VALUE abap_true,
      m_available_modules_expanded TYPE abap_bool VALUE abap_true,
      m_installed_modules_expanded TYPE abap_bool VALUE abap_true,
      m_fullscreen                 TYPE abap_bool VALUE abap_false.

    ALIASES
      fill_container_content FOR if_salv_csqt_content_manager~fill_container_content.

    METHODS:
      constructor IMPORTING ir_sdk_package_manager TYPE REF TO lcl_sdk_package_manager
                  RAISING   lcx_error,
      get_modules_to_be_installed IMPORTING i_target_version                  TYPE string
                                  RETURNING VALUE(rt_modules_to_be_installed) TYPE tt_sdk_tla,
      get_modules_to_be_deleted IMPORTING i_target_version                TYPE string
                                RETURNING VALUE(rt_modules_to_be_deleted) TYPE tt_sdk_tla,
      get_modules_to_be_updated IMPORTING i_target_version                TYPE string
                                RETURNING VALUE(rt_modules_to_be_updated) TYPE tt_sdk_tla,
      save_node_key IMPORTING ir_node_name TYPE string
                              ir_node_key  TYPE salv_de_node_key,
      is_core_installed RETURNING VALUE(r_result) TYPE abap_bool,
      is_fullscreen RETURNING VALUE(r_result) TYPE abap_bool,
      is_sapgui_html RETURNING VALUE(r_result) TYPE abap_bool,
      is_sapgui_timeout_sufficient RETURNING VALUE(r_result) TYPE abap_bool
                                   RAISING   lcx_error,
      is_version_latest IMPORTING i_version       TYPE string
                        RETURNING VALUE(r_result) TYPE abap_bool,
      toggle_inst_modules IMPORTING i_checked TYPE abap_bool,
      display_job_details,
      display_job_details_statusbar,
      fill_fullscreen_content,
      create_container,
      create_fullscreen,
      draw_tree,
      draw_columns,
      draw_nodes,
      draw_node_sdk_core,
      draw_node_installed_modules,
      draw_node_available_modules,
      draw_header,
      draw_footer,
      delete_nodes,
      refresh RAISING lcx_error,
      refresh_nodes,
      refresh_buttons,
      set_handlers,
      set_settings,
      set_functions,
      check_selection_sanity
        IMPORTING it_modules_tbi  TYPE tt_sdk_tla
                  it_modules_tbd  TYPE tt_sdk_tla
                  i_op            TYPE string
                  i_salv_function TYPE syst_ucomm
        RETURNING VALUE(r_result) TYPE abap_bool
        RAISING   lcx_error,
      check_selection_mod_thresholds IMPORTING it_modules_tbi  TYPE tt_sdk_tla
                                               it_modules_tbd  TYPE tt_sdk_tla
                                               i_op            TYPE string
                                               i_salv_function TYPE syst_ucomm
                                     RETURNING VALUE(r_result) TYPE abap_bool
                                     RAISING   lcx_error,
      check_selection_core_validity IMPORTING it_modules_tbi  TYPE tt_sdk_tla
                                              it_modules_tbd  TYPE tt_sdk_tla
                                              i_op            TYPE string
                                              i_salv_function TYPE syst_ucomm
                                    RETURNING VALUE(r_result) TYPE abap_bool,
      check_selection_populated IMPORTING it_modules_tbi  TYPE tt_sdk_tla
                                          it_modules_tbd  TYPE tt_sdk_tla
                                          i_op            TYPE string
                                          i_salv_function TYPE syst_ucomm
                                RETURNING VALUE(r_result) TYPE abap_bool,
      handle_user_command FOR EVENT added_function OF cl_salv_events_tree IMPORTING e_salv_function ,
      handle_checkbox_changed  FOR EVENT checkbox_change OF cl_salv_events_tree IMPORTING node_key columnname checked,
      handle_double_click FOR EVENT double_click OF cl_salv_events_tree IMPORTING node_key columnname,
      handle_on_close FOR EVENT close OF cl_gui_dialogbox_container IMPORTING sender.


  PROTECTED SECTION.
  PRIVATE SECTION.

ENDCLASS.



CLASS lcl_ui_tree_controller IMPLEMENTATION.

  METHOD constructor.

    IF ir_sdk_package_manager IS BOUND.
      mr_sdk_package_manager = ir_sdk_package_manager.
    ELSE.
      mr_sdk_package_manager = NEW lcl_sdk_package_manager( ).
    ENDIF.


    mt_installed_modules = mr_sdk_package_manager->get_sdk_installed_modules( ).

    mt_available_modules_inst = mr_sdk_package_manager->get_sdk_avail_modules_json( i_operation = 'install'
                                                                                    i_source    = 'web'
                                                                                    i_version   = 'LATEST' ).
    mt_available_modules_uninst = mr_sdk_package_manager->get_sdk_avail_modules_json( i_operation = 'uninstall'
                                                                                      i_source    = 'web'
                                                                                      i_version   = 'LATEST' ).

    SELECT tla FROM @mt_available_modules_inst AS am WHERE is_popular = 'X' INTO TABLE @mt_popular_modules .

    " re-hydrate the module staging area if report is restarted
    IMPORT st_modules_to_be_installed = mt_modules_to_be_installed FROM SHARED BUFFER indx(mi) ID 'MOD_INST'.
    IMPORT st_modules_to_be_deleted = mt_modules_to_be_deleted FROM SHARED BUFFER indx(md) ID 'MOD_DELE'.

    " declare popular modules
*    INSERT CONV ts_sdk_module-tla( 'agw' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'ath' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'bdr' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'cmt' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'cpd' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'cwl' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'cwt' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'evb' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'fcs' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'frh' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'gla' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'iot' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'kms' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'kns' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'rek' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'rsd' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'sgm' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'sns' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'sqs' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'ssm' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'tex' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'tnb' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'trl' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'trn' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'xl8' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.
*    INSERT CONV ts_sdk_module-tla( 'lmd' ) INTO TABLE me->mt_popular_modules ##NO_TEXT.


    IF m_fullscreen = abap_false.
      create_container( ).
    ELSE.
      create_fullscreen( ).
    ENDIF.

  ENDMETHOD.


  METHOD draw_tree.

    draw_header( ).

    draw_footer( ).

    draw_columns( ).

    draw_nodes( ).

    set_settings( ).

    set_functions( ).

    set_handlers( ).

    mr_tree->display( ).

  ENDMETHOD.


  METHOD draw_columns.

    DATA: lr_columns TYPE REF TO cl_salv_columns_tree,
          lr_column  TYPE REF TO cl_salv_column_tree.

    lr_columns = mr_tree->get_columns( ).
    lr_columns->set_optimize( abap_true ).

    TRY.
        lr_column ?= lr_columns->get_column( 'TLA' ).
        lr_column->set_long_text( 'TLA' ).
        lr_column->set_medium_text( 'TLA' ).
        lr_column->set_short_text( 'TLA' ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        lr_column->set_visible( abap_false ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'NAME' ).
        lr_column->set_long_text( 'Module name' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Mod. name' ) ##NO_TEXT.
        lr_column->set_short_text( 'Name' ) ##NO_TEXT.
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'CVERS' ).
        lr_column->set_long_text( 'Installed version' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Inst. version' ) ##NO_TEXT.
        lr_column->set_short_text( 'Ins. vers.' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'CTRANSPORT' ).
        lr_column->set_long_text( 'Installed Transport ID' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Inst. Transport ID' ) ##NO_TEXT.
        lr_column->set_short_text( 'Ins. TID' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'TP_RC' ).
        lr_column->set_long_text( 'Last TP Returncode' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Last TP RC' ) ##NO_TEXT.
        lr_column->set_short_text( 'TP RC' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'TP_ICON' ).
        lr_column->set_long_text( '' ).
        lr_column->set_medium_text( '' ).
        lr_column->set_short_text( '' ).
        lr_column->set_icon( abap_true ).
        lr_column->set_alignment( if_salv_c_alignment=>left ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'TP_TEXT' ).
        lr_column->set_long_text( 'Last tp status message' ).
        lr_column->set_medium_text( 'Last tp status msg.' ).
        lr_column->set_short_text( 'Last msg' ).
        lr_column->set_alignment( if_salv_c_alignment=>left ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'AVERS' ).
        lr_column->set_long_text( 'Currently available version' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Available version' ) ##NO_TEXT.
        lr_column->set_short_text( 'Avl. vers.' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'ATRANSPORT' ).
        lr_column->set_long_text( 'Avail Transport ID' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Avail. Transport ID' ) ##NO_TEXT.
        lr_column->set_short_text( 'Avail. TID' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'IS_CORE' ).
        lr_column->set_long_text( 'Is core transport?' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Is core ?' ) ##NO_TEXT.
        lr_column->set_short_text( 'Core?' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        lr_column->set_technical( abap_true ).
        CLEAR lr_column.


        lr_column ?= lr_columns->get_column( 'IS_POPULAR' ).
        lr_column->set_long_text( 'Is popular module?' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Is popular?' ) ##NO_TEXT.
        lr_column->set_short_text( 'Popular?' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        lr_column->set_technical( abap_true ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'OP_ICON' ).
        lr_column->set_long_text( '' ).
        lr_column->set_medium_text( '' ).
        lr_column->set_short_text( '' ).
        lr_column->set_icon( abap_true ).
        lr_column->set_alignment( if_salv_c_alignment=>centered ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'OP_TEXT' ).
        lr_column->set_long_text( 'Planned Operation' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Planned Op.' ) ##NO_TEXT.
        lr_column->set_short_text( 'Operation' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>left ).
        CLEAR lr_column.

        lr_column ?= lr_columns->get_column( 'OP_CODE' ).
        lr_column->set_long_text( 'Operation code' ) ##NO_TEXT.
        lr_column->set_medium_text( 'Operation code' ) ##NO_TEXT.
        lr_column->set_short_text( 'Op. code' ) ##NO_TEXT.
        lr_column->set_alignment( if_salv_c_alignment=>left ).
        lr_column->set_technical( abap_true ).
        CLEAR lr_column.



      CATCH cx_salv_not_found.
    ENDTRY.

  ENDMETHOD.


  METHOD draw_nodes.

    CLEAR mt_folder_node_keys.

    draw_node_sdk_core( ).

    draw_node_installed_modules( ).

    draw_node_available_modules( ).

  ENDMETHOD.


  METHOD draw_node_sdk_core.
    DATA: lr_nodes TYPE REF TO cl_salv_nodes,
          lr_node  TYPE REF TO cl_salv_node,
          lr_item  TYPE REF TO cl_salv_item,
          l_text   TYPE lvc_value,
          lr_key   TYPE salv_de_node_key.


    lr_nodes = mr_tree->get_nodes( ).


* --- ABAP SDK Core folder
    TRY.
        lr_nodes->add_node( EXPORTING related_node = space
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      text         = 'ABAP SDK Core (1)'
                                      folder       = abap_true
                                      expander     = abap_true
                            RECEIVING node         = lr_node ) ##NO_TEXT.
      CATCH cx_salv_msg.
    ENDTRY.



    IF m_sdk_core_expanded = abap_true.
      TRY.
          lr_node->expand( ).
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.


    lr_key = lr_node->get_key( ).

    save_node_key( ir_node_name = |sdk|
                   ir_node_key  = lr_key ).

    DATA wa_core_module TYPE ts_sdk_module.
    READ TABLE mt_installed_modules WITH KEY tla = 'sts' INTO wa_core_module. " core module installed?
    IF sy-subrc <> 0.

    ENDIF.

    wa_core_module-tla = 'core'.
    wa_core_module-name = 'AWS SDK for SAP ABAP core [s3, smr, rla, sts]' ##NO_TEXT.
    wa_core_module-avers = mt_available_modules_inst[ tla = 'core' ]-avers.
    wa_core_module-atransport = mt_available_modules_inst[ tla = 'core' ]-atransport.



    IF wa_core_module-cvers IS NOT INITIAL.

      IF lcl_utils=>cmp_version_string( i_string1 = mt_available_modules_inst[ tla = 'core' ]-avers
                                                    i_string2 = wa_core_module-cvers ) = 0.
        wa_core_module-op_icon = '@08@'.
        wa_core_module-op_text = 'Module up to date, no operation planned.' ##NO_TEXT.
        wa_core_module-op_code = lif_ui_constants=>c_operation_none.
      ELSEIF lcl_utils=>cmp_version_string( i_string1 = mt_available_modules_inst[ tla = 'core' ]-avers
                                                        i_string2 = wa_core_module-cvers ) = 1.
        wa_core_module-op_icon = '@09@'.
        wa_core_module-op_text = 'Module will be updated.' ##NO_TEXT.
        wa_core_module-op_code = lif_ui_constants=>c_operation_update.
      ELSE.
        wa_core_module-op_icon = '@0A@'.
        wa_core_module-op_text = 'Current version higher than available version. Module may be deprecated.' ##NO_TEXT.
        wa_core_module-op_code = lif_ui_constants=>c_operation_none.
      ENDIF.

    ELSE.

      wa_core_module-op_text = 'Will NOT be installed.' ##NO_TEXT.
      wa_core_module-op_code = lif_ui_constants=>c_operation_none.
      wa_core_module-op_icon = '@EB@'.

    ENDIF.

    " Draw processing status when bg job is running
    IF line_exists( mt_modules_to_be_installed[ tla = 'core' ] ) OR
      line_exists( mt_modules_to_be_deleted[ tla = 'core' ] ).

      wa_core_module-tp_icon = '@4A@'.
      wa_core_module-tp_text = 'Processing...' ##NO_TEXT.
      wa_core_module-tp_rc = '8888' ##NO_TEXT.
      wa_core_module-op_icon = '@62@'.

      " is_core_installed( ) does not always work here as not all 4 core modules might
      " be fully present yet during an installation run
      IF is_core_installed( ) = abap_false
       OR wa_core_module-cvers IS NOT INITIAL
       AND line_exists( mt_modules_to_be_installed[ tla = 'core' ] ).
        wa_core_module-op_text = 'Installation operation in progress.' ##NO_TEXT.
      ELSEIF is_core_installed( ) = abap_true
        OR wa_core_module-cvers IS INITIAL
        AND line_exists( mt_modules_to_be_deleted[ tla = 'core' ] ).
        wa_core_module-op_text = 'Delete operation in progress.' ##NO_TEXT.
      ELSE.
        " Do nothing
      ENDIF.

    ENDIF.



    TRY.
        lr_nodes->add_node( EXPORTING related_node = lr_key
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      data_row     = wa_core_module
                                      text         = 'core'
                            RECEIVING node         = lr_node ).
      CATCH cx_salv_msg.
    ENDTRY.


    lr_node->set_collapsed_icon( '@X1@' ). " Changes the doc icon of leaf nodes

    lr_item = lr_node->get_hierarchy_item( ).
    lr_item->set_type( if_salv_c_item_type=>checkbox ).
    lr_item->set_editable( abap_true ).

*    IF is_core_installed( ) = abap_true
*       AND lines( mt_installed_modules ) > 4. "sts, s3, smr, rla
*      lr_item->set_checked( abap_true ).
*      lr_item->set_editable( abap_false ).
*    ELSEIF is_core_installed( ) = abap_true
*      AND lines( mt_installed_modules ) = 4. " core is the only module left
*      lr_item->set_checked( abap_true ).
*      lr_item->set_editable( abap_true ).
*    ELSE. " < 4 modules installed, maybe faulty installation, make checkbox editable for import
*      lr_item->set_checked( abap_false ).
*      lr_item->set_editable( abap_true ).
*    ENDIF.

    IF is_core_installed( ) = abap_true  " core installed and should be updated
     AND line_exists( mt_modules_to_be_installed[ tla = 'core' ] ).
      lr_item->set_checked( abap_true ).
    ELSEIF is_core_installed( ) = abap_true  " core installed and should be deleted
      AND line_exists( mt_modules_to_be_deleted[ tla = 'core' ] ).
      lr_item->set_checked( abap_false ).
    ELSEIF is_core_installed( ) = abap_true   " core installed and should NOT be updated
      AND NOT line_exists( mt_modules_to_be_installed[ tla = 'core' ] ).
      lr_item->set_checked( abap_true ).
    ELSEIF NOT is_core_installed( ) = abap_true   " core NOT installed and should be installed
      AND line_exists( mt_modules_to_be_installed[ tla = 'core' ] ).
      lr_item->set_checked( abap_true ).
    ELSE.
      " Do nothing
    ENDIF.


    IF mr_sdk_package_manager->job_manager->is_job_running( ).
      lr_item->set_editable( abap_false ).
    ENDIF.

  ENDMETHOD.


  METHOD draw_node_installed_modules.

    DATA: lr_nodes TYPE REF TO cl_salv_nodes,
          lr_node  TYPE REF TO cl_salv_node,
          lr_item  TYPE REF TO cl_salv_item,
          l_text   TYPE lvc_value,
          lr_key   TYPE salv_de_node_key.


    lr_nodes = mr_tree->get_nodes( ).

    " TODO: returning the no of modules (installed, available) should go into a dedicated function.
    DATA(l_inst_mod_no) = lines( mt_installed_modules ).
    IF l_inst_mod_no >= 4.
      l_inst_mod_no = l_inst_mod_no - 3.
    ENDIF.

    DATA(l_installed_modules_text) = 'Installed ABAP SDK Modules (' && l_inst_mod_no && ')' ##NO_TEXT.
    DATA(l_installed_modules_lvc) = CONV lvc_value( l_installed_modules_text ).


    TRY.
        lr_nodes->add_node( EXPORTING related_node = space
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      text         = l_installed_modules_lvc
                                      folder       = abap_true
                                      expander     = abap_true
                            RECEIVING node         = lr_node ).
      CATCH cx_salv_msg.
    ENDTRY.



    IF lines( mt_installed_modules ) > 0.
      TRY.
          lr_node->expand( ).
        CATCH cx_salv_msg.
      ENDTRY.
    ELSE.
      TRY.
          lr_node->collapse( ).
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.

    lr_key = lr_node->get_key( ).

    save_node_key( ir_node_name = |installed_modules|
                   ir_node_key  = lr_key ).



    SORT mt_installed_modules BY tla.

    DATA wa_installed_module TYPE ts_sdk_module.

    LOOP AT mt_installed_modules INTO wa_installed_module.

      " Skip modules that are part of AWS_CORE
      IF wa_installed_module-tla = 'sts'
          OR wa_installed_module-tla = 'smr'
          OR wa_installed_module-tla = 's3'
          OR wa_installed_module-tla = 'rla'.

        CONTINUE.

      ELSE.



        wa_installed_module-avers = VALUE #( mt_available_modules_inst[ tla = wa_installed_module-tla ]-avers DEFAULT 'n.a.' ).
        wa_installed_module-atransport = VALUE #( mt_available_modules_inst[ tla = wa_installed_module-tla ]-atransport DEFAULT 'n.a.' ).

        l_text = wa_installed_module-tla.

        IF lcl_utils=>cmp_version_string( i_string1 = wa_installed_module-avers
                                                      i_string2 = wa_installed_module-cvers ) = 0.
          wa_installed_module-op_icon = '@08@'.
          wa_installed_module-op_text = 'Module up to date, no operation planned.' ##NO_TEXT.
          wa_installed_module-op_code = lif_ui_constants=>c_operation_none.
        ELSEIF lcl_utils=>cmp_version_string( i_string1 = wa_installed_module-avers
                                                          i_string2 = wa_installed_module-cvers ) = 1.
          wa_installed_module-op_icon = '@09@'.
          wa_installed_module-op_text = 'Module will be updated.' ##NO_TEXT.
          wa_installed_module-op_code = lif_ui_constants=>c_operation_update.
        ELSE.
          wa_installed_module-op_icon = '@0A@'.
          wa_installed_module-op_text = 'Current version higher than available version. Module may be deprecated.' ##NO_TEXT.
          wa_installed_module-op_code = lif_ui_constants=>c_operation_none.
        ENDIF.




        IF line_exists( mt_modules_to_be_installed[ tla = wa_installed_module-tla ] ) OR
      line_exists( mt_modules_to_be_deleted[ tla = wa_installed_module-tla ] ).

          wa_installed_module-tp_icon = '@4A@'.
          wa_installed_module-tp_text = 'Processing...' ##NO_TEXT.
          wa_installed_module-tp_rc = '8888' ##NO_TEXT.
          wa_installed_module-op_icon = '@62@'.

          IF line_exists( mt_modules_to_be_installed[ tla = wa_installed_module-tla ] ).
            wa_installed_module-op_text = 'Installation operation in progress.'  ##NO_TEXT.
          ELSE.
            wa_installed_module-op_text = 'Delete operation in progress.'  ##NO_TEXT.
          ENDIF.

        ENDIF.



        TRY.
            lr_nodes->add_node( EXPORTING related_node = lr_key
                                          relationship = cl_gui_column_tree=>relat_last_child
                                          data_row     = wa_installed_module
                                          text         = l_text
                                RECEIVING node         = lr_node ).
          CATCH cx_salv_msg.
        ENDTRY.




        lr_node->set_collapsed_icon( '@X1@' ). " Changes the doc icon of leaf nodes


        lr_item = lr_node->get_hierarchy_item( ).
        lr_item->set_type( if_salv_c_item_type=>checkbox ).

        IF line_exists( mt_modules_to_be_deleted[ tla = wa_installed_module-tla ] ).
          lr_item->set_checked( abap_false ).
        ELSE.
          lr_item->set_checked( abap_true ).
        ENDIF.



        IF mr_sdk_package_manager->job_manager->is_job_running( ).
          lr_item->set_editable( abap_false ).
        ELSE.
          lr_item->set_editable( abap_true ).
        ENDIF.

      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD draw_node_available_modules.

    DATA: lr_nodes            TYPE REF TO cl_salv_nodes,
          lr_node             TYPE REF TO cl_salv_node,
          lr_item             TYPE REF TO cl_salv_item,
          l_text              TYPE lvc_value,
          lr_key              TYPE salv_de_node_key,
          lr_popular_mod_key  TYPE salv_de_node_key,
          lr_other_mod_key    TYPE salv_de_node_key,
          lr_popular_mod_node TYPE REF TO cl_salv_node,
          lr_other_mod_node   TYPE REF TO cl_salv_node.


    lr_nodes = mr_tree->get_nodes( ).

    DATA(l_inst_mod_no) = lines( mt_installed_modules ).
    IF l_inst_mod_no >= 4.
      l_inst_mod_no = l_inst_mod_no - 3.
    ENDIF.
    DATA(l_inst_deprecated_mod_ins_no) = lines( mr_sdk_package_manager->get_sdk_deprecated_mod_inst( ) ).
    DATA(l_avail_modules_no) = lines( mt_available_modules_inst ) - ( l_inst_mod_no - l_inst_deprecated_mod_ins_no ).

    IF lines( mt_installed_modules ) = 0.
      l_avail_modules_no = l_avail_modules_no - 4.
    ENDIF.

    "------ Available Modules parent folder
    DATA(l_avail_modules_text) = 'Available ABAP SDK Modules (' && l_avail_modules_no && ')' ##NO_TEXT.
    DATA(l_avail_modules_lvc) = CONV lvc_value( l_avail_modules_text ).

    TRY.
        lr_nodes->add_node( EXPORTING related_node = space
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      text         = l_avail_modules_lvc
                                      folder       = abap_true
                                      expander     = abap_true
                            RECEIVING node         = lr_node ).
      CATCH cx_salv_msg.
    ENDTRY.

    IF m_available_modules_expanded = abap_true.
      TRY.
          lr_node->expand( ).
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.

    lr_key = lr_node->get_key( ).

    save_node_key( ir_node_name = |available_modules|
                   ir_node_key  = lr_key ) ##NO_TEXT.

    "------ Available Modules subfolder Popular Modules
    DATA(l_popular_modules_no) = lines( mt_popular_modules ) - lines( FILTER #( mt_installed_modules IN mt_popular_modules WHERE tla = tla ) ).
    DATA(l_popular_modules_text) = 'Popular Modules (' && l_popular_modules_no && ')' ##NO_TEXT.
    DATA(l_popular_modules_lvc) = CONV lvc_value( l_popular_modules_text ).

    TRY.
        lr_nodes->add_node( EXPORTING related_node = lr_key
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      text         = l_popular_modules_lvc
                                      folder       = abap_true
                                      expander     = abap_true
                            RECEIVING node         = lr_popular_mod_node ).
      CATCH cx_salv_msg.
    ENDTRY.

    IF m_available_modules_expanded = abap_true.
      TRY.
          lr_popular_mod_node->expand( ).
        CATCH cx_salv_msg.
      ENDTRY.
    ENDIF.

    lr_popular_mod_key = lr_popular_mod_node->get_key( ).



    "------ Available Modules subfolder Other Modules
    DATA(l_other_modules_no) = l_avail_modules_no - l_popular_modules_no.
    DATA(l_other_modules_text) = 'Other Modules (' && l_other_modules_no && ')' ##NO_TEXT.
    DATA(l_other_modules_lvc) = CONV lvc_value( l_other_modules_text ).

    TRY.
        lr_nodes->add_node( EXPORTING related_node = lr_key
                                      relationship = cl_gui_column_tree=>relat_last_child
                                      text         = l_other_modules_lvc
                                      folder       = abap_true
                                      expander     = abap_true
                            RECEIVING node         = lr_other_mod_node ).
      CATCH cx_salv_msg.
    ENDTRY.

    lr_other_mod_key = lr_other_mod_node->get_key( ).



    "------ Module list compilation
    SORT mt_available_modules_inst BY tla.

    DATA wa_installed_module TYPE ts_sdk_module.

    LOOP AT mt_available_modules_inst ASSIGNING FIELD-SYMBOL(<fs_available_module>).
      CLEAR: lr_node.

      IF <fs_available_module>-tla = 'core'.

        CONTINUE.

      ELSE.
        CLEAR wa_installed_module.
        READ TABLE mt_installed_modules WITH KEY tla = <fs_available_module>-tla INTO wa_installed_module.
        IF sy-subrc = 0.
          CONTINUE. " Module is already installed, skipping...

        ELSE.

          l_text = <fs_available_module>-tla.
          <fs_available_module>-op_text = 'Will NOT be installed.' ##NO_TEXT.
          <fs_available_module>-op_code = lif_ui_constants=>c_operation_none.
          <fs_available_module>-op_icon = '@EB@'.

          " Draw processing status when bg job is running
          IF line_exists( mt_modules_to_be_installed[ tla = <fs_available_module>-tla ] )
            OR line_exists( mt_modules_to_be_deleted[ tla = <fs_available_module>-tla ] ).
            <fs_available_module>-tp_icon = '@4A@'.
            <fs_available_module>-tp_text = 'Processing...' ##NO_TEXT.
            <fs_available_module>-tp_rc = '8888' ##NO_TEXT.
            <fs_available_module>-op_icon = '@62@'.

            IF line_exists( mt_modules_to_be_installed[ tla = <fs_available_module>-tla ] ).
              <fs_available_module>-op_text = 'Installation operation in progress.'  ##NO_TEXT.
            ELSEIF line_exists( mt_modules_to_be_deleted[ tla = <fs_available_module>-tla ] ).
              <fs_available_module>-op_text = 'Delete operation in progress.'  ##NO_TEXT.
            ELSE.
              " Do nothing
            ENDIF.

          ENDIF.


          TRY.
              IF line_exists( mt_popular_modules[ tla = l_text ] ).

                lr_nodes->add_node( EXPORTING related_node = lr_popular_mod_key
                                              relationship = cl_gui_column_tree=>relat_last_child
                                              data_row     = <fs_available_module>
                                              text         = l_text
                                    RECEIVING node         = lr_node ).
              ELSE.
                lr_nodes->add_node( EXPORTING related_node = lr_other_mod_key
                                              relationship = cl_gui_column_tree=>relat_last_child
                                              data_row     = <fs_available_module>
                                              text         = l_text
                                    RECEIVING node         = lr_node ).

              ENDIF.
            CATCH cx_salv_msg.
          ENDTRY.


          lr_node->set_collapsed_icon( '@X1@' ). " Changes the doc icon of leaf nodes


          lr_item = lr_node->get_hierarchy_item( ).
          lr_item->set_type( if_salv_c_item_type=>checkbox ).

          IF line_exists( mt_modules_to_be_installed[ tla = <fs_available_module>-tla ] ).
            lr_item->set_checked( abap_true ).
          ELSE.
            lr_item->set_checked( abap_false ).
          ENDIF.
          lr_item->set_editable( abap_true ).

*          IF is_core_installed( ) = abap_true.
*
*            lr_item->set_editable( abap_true ).
*
*          ELSE.
*
*            lr_item->set_editable( abap_false ).
*
*          ENDIF.

        ENDIF.
      ENDIF.


      IF mr_sdk_package_manager->job_manager->is_job_running( ).
        lr_item->set_editable( abap_false ).
      ENDIF.

    ENDLOOP.

  ENDMETHOD.


  METHOD save_node_key.

    DATA wa_folder_node_key TYPE ts_named_salv_node_key.
    wa_folder_node_key-node_name = ir_node_name.
    wa_folder_node_key-key = ir_node_key.
    APPEND wa_folder_node_key TO mt_folder_node_keys.

  ENDMETHOD.


  " Draw header with warning if an import job is currently running in the background
  METHOD draw_header.


    " Job already running?
    IF mr_sdk_package_manager->job_manager->is_job_running( ).

      " get job number
      DATA: lt_joblist TYPE tbtcjob_tt.
      lt_joblist = mr_sdk_package_manager->job_manager->get_running_jobs( i_jobname = sy-repid ).
      DATA wa_job TYPE tbtcjob.
      READ TABLE lt_joblist INTO wa_job INDEX 1.
      IF sy-subrc <> 0.
        MESSAGE |Expected to find exactly one job matching { sy-repid } but found { lines( lt_joblist ) }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      ENDIF.

      DATA lr_header_content TYPE REF TO cl_salv_form_header_info.
      DATA l_job_message TYPE string.
      CONCATENATE `Warning: Import background job currently running with number` wa_job-jobcount INTO l_job_message SEPARATED BY ' ' ##NO_TEXT.
      IF is_fullscreen( ).
        lr_header_content = NEW #( text    = l_job_message
                                   tooltip = l_job_message ).
        mr_tree->set_top_of_list( lr_header_content ).
      ELSE.
        MESSAGE l_job_message TYPE 'I' DISPLAY LIKE 'W'.
      ENDIF.

    ENDIF.

  ENDMETHOD.


  " Currently no footer planned
  METHOD draw_footer.

    IF is_fullscreen( ).
      DATA l_message TYPE string.

      DATA lr_footer_content TYPE REF TO cl_salv_form_header_info.


      lr_footer_content = NEW #( text    = l_message
                                 tooltip = l_message ).

      mr_tree->set_end_of_list( lr_footer_content ).

    ENDIF.

  ENDMETHOD.


  METHOD delete_nodes.
    TRY.
        mr_tree->get_nodes( )->delete_all( ).
      CATCH cx_salv_error.
    ENDTRY.
  ENDMETHOD.


  METHOD refresh.

    mt_installed_modules = mr_sdk_package_manager->get_sdk_installed_modules( ).

    mt_available_modules_inst = mr_sdk_package_manager->get_sdk_avail_modules_json( i_operation = 'install'
                                                                                    i_source    = 'web'
                                                                                    i_version   = 'LATEST' ).

    mt_available_modules_uninst = mr_sdk_package_manager->get_sdk_avail_modules_json( i_operation = 'uninstall'
                                                                                      i_source    = 'web'
                                                                                      i_version   = 'LATEST' ).


    IF NOT mr_sdk_package_manager->job_manager->is_job_running( ).
      CLEAR: mt_modules_to_be_installed.
      CLEAR: mt_modules_to_be_deleted.
    ENDIF.


    refresh_nodes( ).

    refresh_buttons( ).

    MESSAGE |Refresh complete.| TYPE 'S' ##NO_TEXT.

    IF mr_sdk_package_manager->job_manager->is_job_running( ).
      display_job_details_statusbar( ).
    ENDIF.

  ENDMETHOD.


  METHOD refresh_nodes.

    delete_nodes( ).

    draw_nodes( ).

  ENDMETHOD.


  METHOD refresh_buttons.

    DATA: lr_functions TYPE REF TO cl_salv_functions_tree,
          lr_function  TYPE REF TO cl_salv_function,
          lt_functions TYPE salv_t_ui_func.

    TRY.
        lr_functions = mr_tree->get_functions( ).

        " If a job is running we deactivate all buttons that could interfere with the running op
        IF mr_sdk_package_manager->job_manager->is_job_running( ) = abap_true.
          lr_functions->enable_function( name    = 'EXE_FGND'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'EXE_BGND'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'INS_FGND'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'INS_BGND'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'DEL_FGND'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'DEL_BGND'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'BTC_DTLS'
                                         boolean = 'X' ).
          lr_functions->enable_function( name    = 'UPD_INST'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'DES_INST'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'DOW_CERT'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'DOW_TRAK'
                                         boolean = ' ' ).
          lr_functions->enable_function( name    = 'INS_ALL'
                                         boolean = ' ' ).

        ELSE. "if not, activate all the buttons

          IF is_sapgui_html( ) = abap_false                   " if we're not in SAPGUI for HTML, AND
            AND is_sapgui_timeout_sufficient( ) = abap_true.  " if we have a sufficiently large (900) auto logout value, foreground processing is enabled
            lr_functions->enable_function( name    = 'EXE_FGND'
                                           boolean = 'X' ).
            lr_functions->enable_function( name    = 'INS_FGND'
                                           boolean = 'X' ).
            lr_functions->enable_function( name    = 'DEL_FGND'
                                           boolean = 'X' ).

          ELSE.
            lr_functions->enable_function( name    = 'EXE_FGND'
                                           boolean = ' ' ).
            lr_functions->enable_function( name    = 'INS_FGND'
                                           boolean = ' ' ).
            lr_functions->enable_function( name    = 'DEL_FGND'
                                           boolean = ' ' ).

          ENDIF.
          lr_functions->enable_function( name    = 'EXE_BGND'
                                         boolean = 'X' ).
          lr_functions->enable_function( name    = 'DEL_BGND'
                                         boolean = 'X' ).
          lr_functions->enable_function( name    = 'INS_BGND'
                                         boolean = 'X' ).
          lr_functions->enable_function( name    = 'BTC_DTLS'
                                         boolean = ' ' ).

          IF lines( mt_installed_modules ) > 4.  " if core is installed, activate the select/deslected buttons
            lr_functions->enable_function( name    = 'UPD_INST'
                                           boolean = 'X' ).
            lr_functions->enable_function( name    = 'DES_INST'
                                           boolean = 'X' ).
          ELSE.
            lr_functions->enable_function( name    = 'UPD_INST'
                                           boolean = ' ' ).
            lr_functions->enable_function( name    = 'DES_INST'
                                           boolean = ' ' ).
          ENDIF.

          lr_functions->enable_function( name    = 'DOW_CERT'
                                         boolean = 'X' ).
          lr_functions->enable_function( name    = 'DOW_TRAK'
                                         boolean = 'X' ).
          lr_functions->enable_function( name    = 'INS_ALL'
                                         boolean = 'X' ).
        ENDIF.
      CATCH cx_salv_wrong_call INTO DATA(r_ex1).
        MESSAGE r_ex1->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
      CATCH cx_salv_not_found INTO DATA(r_ex2).
        MESSAGE r_ex2->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
      CATCH cx_root INTO DATA(r_ex).
        MESSAGE r_ex->get_text( ) TYPE 'I' DISPLAY LIKE 'E'.
    ENDTRY.

  ENDMETHOD.


  METHOD set_handlers.
    DATA lr_tree_events TYPE REF TO cl_salv_events_tree.

    lr_tree_events = mr_tree->get_event( ).

    SET HANDLER handle_user_command FOR lr_tree_events.
    SET HANDLER handle_checkbox_changed FOR lr_tree_events.
    SET HANDLER handle_double_click FOR lr_tree_events.

  ENDMETHOD.


  METHOD set_settings.

    DATA lr_settings  TYPE REF TO cl_salv_tree_settings.
    lr_settings = mr_tree->get_tree_settings( ).
    lr_settings->set_hierarchy_header( 'Modules' ) ##NO_TEXT.

    IF is_fullscreen( ).
      lr_settings->set_header( 'AWS ABAP SDK package control (fullscreen)' ) ##NO_TEXT.
    ENDIF.

  ENDMETHOD.


  METHOD display_job_details_statusbar.

    " Job already running?
    IF mr_sdk_package_manager->job_manager->is_job_running( ).

      " get job number
      DATA: lt_joblist TYPE tbtcjob_tt.
      lt_joblist = mr_sdk_package_manager->job_manager->get_running_jobs( i_jobname = sy-repid ).
      DATA wa_job TYPE tbtcjob.
      READ TABLE lt_joblist INTO wa_job INDEX 1.
      IF sy-subrc <> 0.
        MESSAGE |Expected to find exactly one job matching { sy-repid } but found { lines( lt_joblist ) }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      ENDIF.
      DATA l_job_message TYPE string.
      CONCATENATE `Import background job currently running with number` wa_job-jobcount INTO l_job_message SEPARATED BY ' ' ##NO_TEXT.

      MESSAGE l_job_message TYPE 'S' DISPLAY LIKE 'W'.

    ENDIF.

  ENDMETHOD.


  METHOD display_job_details.

    " Job already running?
    IF mr_sdk_package_manager->job_manager->is_job_running( ).

      " get job number
      DATA: lt_joblist TYPE tbtcjob_tt.
      lt_joblist = mr_sdk_package_manager->job_manager->get_running_jobs( i_jobname = sy-repid ).
      DATA wa_job TYPE tbtcjob.
      READ TABLE lt_joblist INTO wa_job INDEX 1.
      IF sy-subrc <> 0.
        MESSAGE |Expected to find exactly one job matching { sy-repid } but found { lines( lt_joblist ) }| TYPE 'I' DISPLAY LIKE 'E' ##NO_TEXT.
      ENDIF.

      DATA l_job_message TYPE string.
      CONCATENATE `Import background job currently running with number` wa_job-jobcount INTO l_job_message SEPARATED BY ' ' ##NO_TEXT.

      MESSAGE l_job_message TYPE 'I'.
    ELSE.
      MESSAGE 'No background job currently running.' TYPE 'I' ##NO_TEXT.

    ENDIF.

  ENDMETHOD.


  METHOD set_functions.

    DATA: lr_functions TYPE REF TO cl_salv_functions_tree,
          lr_function  TYPE REF TO cl_salv_function,
          lt_functions TYPE salv_t_ui_func.

    lr_functions = mr_tree->get_functions( ).
    lr_functions->set_all( abap_true ).

    TRY.

        lr_functions->add_function(
          name     = 'REF_TREE'
          icon     = '@42@'
          text     = 'Refresh'
          tooltip  = 'Refresh the whole tree view'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'EXE_FGND'
          icon     = '@15@'
          text     = 'Execute all'
          tooltip  = 'Start operations in dialog processing'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'EXE_BGND'
          icon     = '@15@'
          text     = 'Execute all (batch)'
          tooltip  = 'Start operations in batch processing'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'INS_FGND'
          icon     = '@KA@'
          text     = 'Install only'
          tooltip  = 'Install only in dialog processing'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'INS_BGND'
          icon     = '@KA@'
          text     = 'Install only (batch)'
          tooltip  = 'Install only in batch processing'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'DEL_FGND'
          icon     = '@11@'
          text     = 'Delete only'
          tooltip  = 'Delete only in dialog processing'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'DEL_BGND'
          icon     = '@11@'
          text     = 'Delete only (batch)'
          tooltip  = 'Delete only in batch processing'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'BTC_DTLS'
          icon     = '@M5@'
          text     = 'Job info'
          tooltip  = 'Details on a running background job (if any)'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'UPD_INST'
          icon     = '@B1@'
          text     = 'Select installed modules'
          tooltip  = 'Mark all installed modules for update if applicable'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'DES_INST'
          icon     = '@B2@'
          text     = 'Deselect installed modules'
          tooltip  = 'Mark all installed modules for deletion'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'DOW_CERT'
          icon     = '@XL@'
          text     = 'Install SSL Certificates'
          tooltip  = 'Install Amazon Root Certificates'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'DOW_TRAK'
          icon     = '@X1@'
          text     = 'Download current SDK'
          tooltip  = 'Download current ABAP SDK Transport Kits'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        lr_functions->add_function(
          name     = 'INS_ALL'
          icon     = '@6N@'
          text     = 'Install all modules'
          tooltip  = 'Install all available modules'
          position = if_salv_c_function_position=>left_of_salv_functions ) ##NO_TEXT.

        refresh_buttons( ).

      CATCH cx_salv_existing.
      CATCH cx_salv_wrong_call.
    ENDTRY.


  ENDMETHOD.


  METHOD fill_fullscreen_content.

    TRY.
        cl_salv_tree=>factory( EXPORTING hide_header = abap_false IMPORTING r_salv_tree = mr_tree CHANGING t_table = mt_treetab ).
      CATCH cx_salv_error.
    ENDTRY.

    draw_tree( ).

  ENDMETHOD.


  METHOD fill_container_content.

    TRY.
        cl_salv_tree=>factory( EXPORTING r_container = r_container
                                         hide_header = abap_false
                               IMPORTING r_salv_tree = mr_tree
                               CHANGING  t_table     = mt_treetab ).
      CATCH cx_salv_error.
    ENDTRY.

    mr_container = r_container.

    draw_tree( ).

  ENDMETHOD.


  METHOD create_fullscreen.

    fill_fullscreen_content( ).

  ENDMETHOD.

  METHOD create_container.

    CALL FUNCTION 'SALV_CSQT_CREATE_CONTAINER'
      EXPORTING
        r_content_manager = me
        title             = 'ABAP SDK Package Manager' ##NO_TEXT.

  ENDMETHOD.

  METHOD is_core_installed.

    DATA: lv_result TYPE abap_bool VALUE abap_false.

    IF line_exists( mt_installed_modules[ tla = 'sts' ] )
      AND line_exists( mt_installed_modules[ tla = 's3' ] )
      AND line_exists( mt_installed_modules[ tla = 'smr' ] )
      AND line_exists( mt_installed_modules[ tla = 'rla' ] ).
      lv_result = abap_true.
    ENDIF.

    r_result = lv_result.

  ENDMETHOD.

  METHOD is_version_latest.

    TRY.
        DATA(lt_latest_inst_modules) = mr_sdk_package_manager->get_sdk_avail_modules_json( i_operation = 'install'
                                                                                           i_source    = 'web'
                                                                                           i_version   = 'LATEST' ).
      CATCH lcx_error.
    ENDTRY.

    IF i_version = lt_latest_inst_modules[ tla = 'core' ]-avers.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD is_fullscreen.

    DATA lr_gui_container TYPE REF TO cl_gui_container.

    IF mr_container IS NOT INITIAL.
      lr_gui_container = mr_container.
    ELSE.
      lr_gui_container = CAST #( cl_gui_container=>screen0->children[ 1 ] ).
    ENDIF.

    IF lr_gui_container->get_name( ) = 'TREE_CONTAINER'.
      r_result = abap_true.
    ELSE.
      r_result = abap_false.
    ENDIF.

  ENDMETHOD.

  METHOD is_sapgui_html.

    DATA: lv_result TYPE abap_bool VALUE abap_false.

    CALL FUNCTION 'GUI_IS_ITS'
      IMPORTING
        return = lv_result.

    r_result = lv_result.

  ENDMETHOD.

  METHOD is_sapgui_timeout_sufficient.

    DATA: lv_gui_auto_timeout TYPE spfpflpar-pvalue VALUE '0'.
    DATA: lv_result TYPE abap_bool VALUE abap_false.

    CALL FUNCTION 'RSAN_SYSTEM_PARAMETER_READ'
      EXPORTING
        i_name     = 'rdisp/gui_auto_logout'
      IMPORTING
        e_value    = lv_gui_auto_timeout
      EXCEPTIONS
        read_error = 1
        OTHERS     = 2.
    IF sy-subrc <> 0.
      RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not find text for popup| ##NO_TEXT.
    ENDIF.

    IF lv_gui_auto_timeout > lif_ui_constants=>c_sapgui_autologout_threshold.
      lv_result = abap_true.
    ENDIF.

    r_result = lv_result.

  ENDMETHOD.


  METHOD get_modules_to_be_installed.
    DATA lr_selections TYPE REF TO cl_salv_selections_tree.
    DATA lr_nodes TYPE REF TO cl_salv_nodes.
    DATA lt_nodes TYPE salv_t_nodes.

    TRY.
        lr_nodes = mr_tree->get_nodes( ).
      CATCH cx_salv_msg.
    ENDTRY.

    TRY.
        lt_nodes = lr_nodes->get_all_nodes( ).
      CATCH cx_salv_msg.
    ENDTRY.


    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<fs_node>).

      TRY.

          IF <fs_node>-node->get_hierarchy_item( )->is_checked( )
            AND NOT <fs_node>-node->is_folder( )
            AND CAST string( <fs_node>-node->get_item( 'OP_CODE' )->get_value( ) )->* = lif_ui_constants=>c_operation_install.

            IF <fs_node>-node->get_text( ) = 'sts' OR
                <fs_node>-node->get_text( ) = 's3' OR
                <fs_node>-node->get_text( ) = 'smr' OR
                <fs_node>-node->get_text( ) = 'rla'.

              IF line_exists( rt_modules_to_be_installed[ 'core' ] ).
                CONTINUE.
              ELSE.
                INSERT VALUE ts_sdk_tla( tla = 'core' version = i_target_version ) INTO TABLE rt_modules_to_be_installed.
              ENDIF.

            ELSE.
              INSERT VALUE ts_sdk_tla( tla = <fs_node>-node->get_text( ) version = i_target_version ) INTO TABLE rt_modules_to_be_installed.
            ENDIF.
          ENDIF.

        CATCH cx_salv_msg.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.


  METHOD get_modules_to_be_deleted.
    DATA lr_selections TYPE REF TO cl_salv_selections_tree.
    DATA lr_nodes TYPE REF TO cl_salv_nodes.
    DATA lt_nodes TYPE salv_t_nodes.

    TRY.
        lr_nodes = mr_tree->get_nodes( ).
      CATCH cx_salv_msg.
    ENDTRY.

    TRY.
        lt_nodes = lr_nodes->get_all_nodes( ).
      CATCH cx_salv_msg.
    ENDTRY.

    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<fs_node>).

      TRY.

          IF NOT <fs_node>-node->get_hierarchy_item( )->is_checked( )
          AND NOT <fs_node>-node->is_folder( )
          AND CAST string( <fs_node>-node->get_item( 'OP_CODE' )->get_value( ) )->* = lif_ui_constants=>c_operation_delete.

            IF <fs_node>-node->get_text( ) = 'sts' OR
                <fs_node>-node->get_text( ) = 's3' OR
                <fs_node>-node->get_text( ) = 'smr' OR
                <fs_node>-node->get_text( ) = 'rla'.

              IF line_exists( rt_modules_to_be_deleted[ 'core' ] ).
                CONTINUE.
              ELSE.
                INSERT VALUE ts_sdk_tla( tla = 'core' version = i_target_version ) INTO TABLE rt_modules_to_be_deleted.
              ENDIF.

            ELSE.
              INSERT VALUE ts_sdk_tla( tla = <fs_node>-node->get_text( ) version = i_target_version ) INTO TABLE rt_modules_to_be_deleted.
            ENDIF.

          ENDIF.



        CATCH cx_salv_msg.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.

  METHOD get_modules_to_be_updated.
    DATA lr_selections TYPE REF TO cl_salv_selections_tree.
    DATA lr_nodes TYPE REF TO cl_salv_nodes.
    DATA lt_nodes TYPE salv_t_nodes.

    TRY.
        lr_nodes = mr_tree->get_nodes( ).
      CATCH cx_salv_msg.
    ENDTRY.

    TRY.
        lt_nodes = lr_nodes->get_all_nodes( ).
      CATCH cx_salv_msg.
    ENDTRY.


    LOOP AT lt_nodes ASSIGNING FIELD-SYMBOL(<fs_node>).

      TRY.

          IF <fs_node>-node->get_hierarchy_item( )->is_checked( )
            AND NOT <fs_node>-node->is_folder( )
            AND CAST string( <fs_node>-node->get_item( 'OP_CODE' )->get_value( ) )->* = lif_ui_constants=>c_operation_update.

            IF <fs_node>-node->get_text( ) = 'sts' OR
                <fs_node>-node->get_text( ) = 's3' OR
                <fs_node>-node->get_text( ) = 'smr' OR
                <fs_node>-node->get_text( ) = 'rla'.
              INSERT VALUE ts_sdk_tla( tla = 'core' version = i_target_version ) INTO TABLE rt_modules_to_be_updated.
            ELSE.
              INSERT VALUE ts_sdk_tla( tla = <fs_node>-node->get_text( ) version = i_target_version ) INTO TABLE rt_modules_to_be_updated.
            ENDIF.
          ENDIF.

        CATCH cx_salv_msg.
      ENDTRY.

    ENDLOOP.
  ENDMETHOD.


  METHOD check_selection_mod_thresholds.

    DATA lv_text TYPE string.
    DATA lv_answer TYPE c.

    CASE i_op.

      WHEN 'install'.
        DATA(lv_number_modules_inst) = lines( it_modules_tbi ).
        DATA(lv_number_new_modules) = 0.
        DATA(lv_number_modules_avail_inst) = lines( mt_available_modules_inst ).

        " determine modules to be net newly installed
        LOOP AT it_modules_tbi INTO DATA(wa_module_tbi).
          IF ( wa_module_tbi-tla <> 'core' ) AND NOT line_exists( mt_installed_modules[ tla = wa_module_tbi-tla ] ).
            lv_number_new_modules = lv_number_new_modules + 1.
            lv_number_modules_inst = lv_number_modules_inst - 1.
          ENDIF.

          IF ( wa_module_tbi-tla = 'core' ) AND ( NOT is_core_installed( ) ).
            lv_number_new_modules = lv_number_new_modules + 1.
            lv_number_modules_inst = lv_number_modules_inst - 1.
          ENDIF.

        ENDLOOP.


        IF ( lv_number_modules_inst + lv_number_new_modules ) >= lv_number_modules_avail_inst.
          lv_text = |You have selected all { lv_number_modules_inst + lv_number_new_modules } modules available for installation. |
                 && |Having all modules installed is discouraged and should only be done in demo systems. |
                 && |Continue? | ##NO_TEXT.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = ' '
              diagnose_object       = ' '
              text_question         = lv_text
              text_button_1         = |Do it anyway|
              icon_button_1         = ' '
              text_button_2         = |Go back|
              icon_button_2         = ' '
              default_button        = '2'
              display_cancel_button = ' '
              userdefined_f1_help   = ' '
              start_column          = 25
              start_row             = 6
              iv_quickinfo_button_1 = ' '
              iv_quickinfo_button_2 = ' '
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2 ##NO_TEXT.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not find text for popup| ##NO_TEXT.
          ENDIF.

          IF lv_answer = 1.
            r_result = abap_true.
          ELSE.
            r_result = abap_false.
            RETURN.
          ENDIF.

        ENDIF.

        IF ( lv_number_new_modules > lif_ui_constants=>c_batch_rec_threshold OR lv_number_modules_inst > lif_ui_constants=>c_batch_rec_threshold )
            AND ( i_salv_function = 'STRT' OR i_salv_function = 'EXE_FGND' OR i_salv_function = 'INS_FGND' ).
          lv_text = |You have selected { lv_number_new_modules + lv_number_modules_inst } modules for installation (or update). |
                 & |This can take a while (using batch mode is recommended). |
                 & |Continue with foreground mode? | ##NO_TEXT.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = ' '
              diagnose_object       = ' '
              text_question         = lv_text
              text_button_1         = |Yes, please|
              icon_button_1         = ' '
              text_button_2         = |Go back|
              icon_button_2         = ' '
              default_button        = '2'
              display_cancel_button = ' '
              userdefined_f1_help   = ' '
              start_column          = 25
              start_row             = 6
              iv_quickinfo_button_1 = ' '
              iv_quickinfo_button_2 = ' '
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2 ##NO_TEXT.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not find text for popup| ##NO_TEXT.
          ENDIF.

          IF lv_answer = 1.
            r_result = abap_true.
          ELSE.
            r_result = abap_false.
          ENDIF.

        ELSE.
          r_result = abap_true.
        ENDIF.

        RETURN.

      WHEN 'uninstall'.

        DATA(lv_number_modules_tbd) = lines( it_modules_tbd ).

        IF lv_number_modules_tbd > lif_ui_constants=>c_batch_rec_threshold
            AND ( i_salv_function = 'STRT' OR i_salv_function = 'EXE_FGND' OR i_salv_function = 'DEL_FGND' ).
          lv_text = |You have selected { lv_number_modules_tbd } modules for deletion. |
                         && |This can take a while (using batch mode is recommended). |
                         && |Continue in foreground mode? | ##NO_TEXT.

          CALL FUNCTION 'POPUP_TO_CONFIRM'
            EXPORTING
              titlebar              = ' '
              diagnose_object       = ' '
              text_question         = lv_text
              text_button_1         = |Yes, please|
              icon_button_1         = ' '
              text_button_2         = |Go back|
              icon_button_2         = ' '
              default_button        = '2'
              display_cancel_button = ' '
              userdefined_f1_help   = ' '
              start_column          = 25
              start_row             = 6
              iv_quickinfo_button_1 = ' '
              iv_quickinfo_button_2 = ' '
            IMPORTING
              answer                = lv_answer
            EXCEPTIONS
              text_not_found        = 1
              OTHERS                = 2 ##NO_TEXT.
          IF sy-subrc <> 0.
            RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not find text for popup| ##NO_TEXT.
          ENDIF.

          IF lv_answer = 1.
            r_result = abap_true.
          ELSE.
            r_result = abap_false.
          ENDIF.

        ELSE.
          r_result = abap_true.
        ENDIF.

        RETURN.
      WHEN OTHERS.

    ENDCASE.

  ENDMETHOD.

  METHOD check_selection_core_validity.

    DATA: lv_result TYPE abap_bool VALUE abap_false.

    CASE i_op.

      WHEN 'install'.

        IF is_core_installed( ) = abap_false                            " core is NOT installed
          AND NOT line_exists( it_modules_tbi[ tla = 'core' ] )  " and NOT selected for installation
          AND lines( it_modules_tbi ) > 0.                              " are other modules selected for installation?
          MESSAGE |Please install core module with first deployment.| TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
          lv_result = abap_false.                                       " then block the request

        ELSEIF is_core_installed( ) = abap_true                         " core is installed
            AND line_exists( it_modules_tbd[ tla = 'core' ] )    " and marked for deletion
            AND ( lines( it_modules_tbd ) < lines( mt_installed_modules ) - 3    " are any installed modules NOT selected for deletion?
            OR lines( it_modules_tbi ) > 0 ).                                   " or did the user select additional modules for installation?
          MESSAGE |To delete core, additionally mark all installed modules for deletion.| TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
          lv_result = abap_false.                                       " then block the request

        ELSE.
          lv_result = abap_true.
        ENDIF.

      WHEN 'uninstall'.

        IF is_core_installed( ) = abap_true                           " core is installed
            AND line_exists( it_modules_tbd[ tla = 'core' ] )  " and marked for deletion
            AND ( lines( it_modules_tbd ) < lines( mt_installed_modules ) - 3 " are any installed modules NOT selected for deletion?
            OR lines( it_modules_tbi ) > 0 ).                                " or did the user select additional modules for installation?
          MESSAGE |To delete core, additionally mark all installed modules for deletion.| TYPE 'S' DISPLAY LIKE 'E' ##NO_TEXT.
          lv_result = abap_false.                                           " then block the request
        ELSE.
          lv_result = abap_true.
        ENDIF.

    ENDCASE.

    r_result = lv_result.

  ENDMETHOD.


  METHOD check_selection_populated.

    DATA: lv_result TYPE abap_bool VALUE abap_false.

    CASE i_op.

      WHEN 'install'.

        IF lines( it_modules_tbi ) + lines( it_modules_tbd ) > 0.
          lv_result = abap_true.
        ELSE.
          MESSAGE |Nothing to do.| TYPE 'S' ##NO_TEXT.
          lv_result = abap_false.
        ENDIF.

      WHEN 'uninstall'.

        IF lines( it_modules_tbd ) > 0.
          lv_result = abap_true.
        ELSE.
          MESSAGE |Nothing to do.| TYPE 'S' ##NO_TEXT.
          lv_result = abap_false.
        ENDIF.

    ENDCASE.

    r_result = lv_result.

  ENDMETHOD.


  METHOD check_selection_sanity.

    DATA: lv_result_populated TYPE abap_bool VALUE abap_false.
    DATA: lv_result_core_validity TYPE abap_bool VALUE abap_false.
    DATA: lv_result_mod_thresholds TYPE abap_bool VALUE abap_false.

    lv_result_populated = check_selection_populated( it_modules_tbi  = it_modules_tbi
                                                     it_modules_tbd  = it_modules_tbd
                                                     i_op            = i_op
                                                     i_salv_function = i_salv_function ).

    IF lv_result_populated = abap_false.
      r_result = abap_false.
      RETURN.
    ENDIF.

    lv_result_core_validity = check_selection_core_validity( it_modules_tbi  = it_modules_tbi
                                                             it_modules_tbd  = it_modules_tbd
                                                             i_op            = i_op
                                                             i_salv_function = i_salv_function ).

    IF lv_result_core_validity = abap_false.
      r_result = abap_false.
      RETURN.
    ENDIF.

    lv_result_mod_thresholds = check_selection_mod_thresholds( it_modules_tbi  = it_modules_tbi
                                                               it_modules_tbd  = it_modules_tbd
                                                               i_op            = i_op
                                                               i_salv_function = i_salv_function ).

    IF lv_result_mod_thresholds = abap_false.
      r_result = abap_false.
      RETURN.
    ENDIF.


    r_result = abap_true.


  ENDMETHOD.


  METHOD handle_user_command.


    DATA: l_job_message TYPE string.
    DATA: l_jobnumber TYPE btcjobcnt.
    DATA: lv_target_version TYPE string.

    DATA(lv_ucomm) = sy-ucomm.

    DATA(lv_selection) = mr_tree->get_selections( ).

    TRY.
        CASE lv_ucomm.
          WHEN 'BACK' OR 'LEAR' OR 'CANC'.
            LEAVE PROGRAM.
        ENDCASE.

        CASE e_salv_function.
          WHEN 'STRT' OR 'EXE_FGND'.

            " we update everything to the latest version
            lv_target_version = 'LATEST'.

            CLEAR: mt_modules_to_be_installed.
            CLEAR: mt_modules_to_be_deleted.
            CLEAR: mt_modules_to_be_updated.
            mt_modules_to_be_installed = get_modules_to_be_installed( i_target_version = lv_target_version ).
            mt_modules_to_be_deleted = get_modules_to_be_deleted( i_target_version = lv_target_version ).
            mt_modules_to_be_updated = get_modules_to_be_updated( i_target_version = lv_target_version ).

            INSERT LINES OF mt_modules_to_be_updated INTO TABLE mt_modules_to_be_installed.

            IF check_selection_sanity( it_modules_tbi = mt_modules_to_be_installed
                                       it_modules_tbd = mt_modules_to_be_deleted
                                       i_salv_function = e_salv_function
                                       i_op = 'install' ) = abap_false.
              RETURN.
            ENDIF.

            IF lines( mt_modules_to_be_installed ) > 0
              OR lines( mt_modules_to_be_deleted ) > 0
              OR lines( mt_modules_to_be_updated ) > 0.
              IF mr_sdk_package_manager->sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
                RETURN.
              ENDIF.

              IF mr_sdk_package_manager->update_zipfiles_if_outdated( i_avers_core_inst = mt_available_modules_inst[ tla = 'core' ]-avers
                                      i_avers_core_uninst = mt_available_modules_uninst[ tla = 'core' ]-avers ) = abap_false.
                RETURN.
              ENDIF.

            ENDIF.

            mr_sdk_package_manager->run_foreground( i_modules_to_be_installed = mt_modules_to_be_installed
                                                    i_modules_to_be_deleted   = mt_modules_to_be_deleted
                                                    i_target_version          = lv_target_version ).
            refresh( ).

          WHEN 'BTCH' OR 'EXE_BGND'.

            " we update everything to the latest version
            lv_target_version = 'LATEST'.

            CLEAR: mt_modules_to_be_installed.
            CLEAR: mt_modules_to_be_deleted.
            CLEAR: mt_modules_to_be_updated.
            mt_modules_to_be_installed = get_modules_to_be_installed( i_target_version = lv_target_version ).
            mt_modules_to_be_deleted = get_modules_to_be_deleted( i_target_version = lv_target_version ).
            mt_modules_to_be_updated = get_modules_to_be_updated( i_target_version = lv_target_version ).

            INSERT LINES OF mt_modules_to_be_updated INTO TABLE mt_modules_to_be_installed.

            IF check_selection_sanity( it_modules_tbi = mt_modules_to_be_installed
                                      it_modules_tbd = mt_modules_to_be_deleted
                                      i_salv_function = e_salv_function
                                      i_op = 'install' ) = abap_false.
              RETURN.
            ENDIF.


            IF lines( mt_modules_to_be_installed ) > 0
              OR lines( mt_modules_to_be_deleted ) > 0
              OR lines( mt_modules_to_be_updated ) > 0.
              IF mr_sdk_package_manager->sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
                RETURN.
              ENDIF.

              IF mr_sdk_package_manager->update_zipfiles_if_outdated( i_avers_core_inst = mt_available_modules_inst[ tla = 'core' ]-avers
                                      i_avers_core_uninst = mt_available_modules_uninst[ tla = 'core' ]-avers ) = abap_false.
                RETURN.
              ENDIF.

            ENDIF.



            l_jobnumber = mr_sdk_package_manager->job_manager->submit_batch_job( i_modules_to_be_installed = mt_modules_to_be_installed
                                                                                 i_modules_to_be_deleted   = mt_modules_to_be_deleted
                                                                                 i_target_version          = lv_target_version ).

            CONCATENATE `Submitted job with number ` l_jobnumber INTO l_job_message ##NO_TEXT.
            MESSAGE i000(0k) WITH l_job_message.

            refresh( ).

          WHEN 'INS_FGND'.

            " we only use the same version as core (given it is installed at all)
            " if the installed version of core is the NOT the latest version
            IF is_core_installed( ) = abap_true.
              IF NOT is_version_latest( mt_installed_modules[ tla = 'sts' ]-cvers ).
                lv_target_version = mt_installed_modules[ tla = 'sts' ]-cvers.
              ELSE.
                lv_target_version = 'LATEST'.
              ENDIF.
            ELSE.
              lv_target_version = 'LATEST'.
            ENDIF.

            CLEAR: mt_modules_to_be_installed.
            CLEAR: mt_modules_to_be_deleted.
            mt_modules_to_be_installed = get_modules_to_be_installed( i_target_version = lv_target_version ).


            IF check_selection_sanity( it_modules_tbi = mt_modules_to_be_installed
                                       it_modules_tbd = mt_modules_to_be_deleted
                                       i_salv_function = e_salv_function
                                       i_op = 'install' ) = abap_false.
              RETURN.
            ENDIF.

            IF lines( mt_modules_to_be_installed ) > 0.

              IF mr_sdk_package_manager->sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
                RETURN.
              ENDIF.

            ENDIF.

            mr_sdk_package_manager->run_foreground( i_modules_to_be_installed = mt_modules_to_be_installed
                                                    i_modules_to_be_deleted   = mt_modules_to_be_deleted
                                                    i_target_version          = lv_target_version ). " empty since modules tbd are not collected here
            refresh( ).

          WHEN 'INS_BGND'.

            " we only use the same version as core (given it is installed at all)
            " if the installed version of core is the NOT the latest version
            IF is_core_installed( ) = abap_true.
              IF NOT is_version_latest( mt_installed_modules[ tla = 'sts' ]-cvers ).
                lv_target_version = mt_installed_modules[ tla = 'sts' ]-cvers.
              ELSE.
                lv_target_version = 'LATEST'.
              ENDIF.
            ELSE.
              lv_target_version = 'LATEST'.
            ENDIF.


            CLEAR: mt_modules_to_be_installed.
            CLEAR: mt_modules_to_be_deleted.
            mt_modules_to_be_installed = get_modules_to_be_installed( i_target_version = lv_target_version ).


            IF check_selection_sanity( it_modules_tbi = mt_modules_to_be_installed
                                                   it_modules_tbd = mt_modules_to_be_deleted
                                                   i_salv_function = e_salv_function
                                                   i_op = 'install' ) = abap_false.
              RETURN.
            ENDIF.

            IF lines( mt_modules_to_be_installed ) > 0.

              IF mr_sdk_package_manager->sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
                RETURN.
              ENDIF.

            ENDIF.

            l_jobnumber = mr_sdk_package_manager->job_manager->submit_batch_job( i_modules_to_be_installed = mt_modules_to_be_installed
                                                                                 i_modules_to_be_deleted   = mt_modules_to_be_deleted
                                                                                 i_target_version          = lv_target_version ). " empty since modules tbd are not collected here

            CONCATENATE `Submitted job with number ` l_jobnumber INTO l_job_message ##NO_TEXT.
            MESSAGE i000(0k) WITH l_job_message.

            refresh( ).

          WHEN 'DEL_FGND'.

            " we only use the same version as core (given it is installed at all)
            " if the installed version of core is the NOT the latest version
            IF is_core_installed( ) = abap_true.
              IF NOT is_version_latest( mt_installed_modules[ tla = 'sts' ]-cvers ).
                lv_target_version = mt_installed_modules[ tla = 'sts' ]-cvers.
              ELSE.
                lv_target_version = 'LATEST'.
              ENDIF.
            ELSE.
              lv_target_version = 'LATEST'.
            ENDIF.


            CLEAR: mt_modules_to_be_installed.
            CLEAR: mt_modules_to_be_deleted.
            mt_modules_to_be_deleted = get_modules_to_be_deleted( i_target_version = lv_target_version ).


            IF check_selection_sanity( it_modules_tbi = mt_modules_to_be_installed
                                       it_modules_tbd = mt_modules_to_be_deleted
                                       i_salv_function = e_salv_function
                                       i_op = 'uninstall' ) = abap_false.
              RETURN.
            ENDIF.

            IF lines( mt_modules_to_be_deleted ) > 0.

              IF mr_sdk_package_manager->sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
                RETURN.
              ENDIF.

            ENDIF.

            mr_sdk_package_manager->run_foreground( i_modules_to_be_installed = mt_modules_to_be_installed " empty since modules tbi are not collected here
                                                    i_modules_to_be_deleted   = mt_modules_to_be_deleted
                                                    i_target_version          = lv_target_version ).
            refresh( ).

          WHEN 'DEL_BGND'.

            " we only use the same version as core (given it is installed at all)
            " if the installed version of core is the NOT the latest version
            IF is_core_installed( ) = abap_true.
              IF NOT is_version_latest( mt_installed_modules[ tla = 'sts' ]-cvers ).
                lv_target_version = mt_installed_modules[ tla = 'sts' ]-cvers.
              ELSE.
                lv_target_version = 'LATEST'.
              ENDIF.
            ELSE.
              lv_target_version = 'LATEST'.
            ENDIF.


            CLEAR: mt_modules_to_be_installed.
            CLEAR: mt_modules_to_be_deleted.
            mt_modules_to_be_deleted = get_modules_to_be_deleted( i_target_version = lv_target_version ).


            IF check_selection_sanity( it_modules_tbi = mt_modules_to_be_installed
                                                   it_modules_tbd = mt_modules_to_be_deleted
                                                   i_salv_function = e_salv_function
                                                   i_op = 'uninstall' ) = abap_false.
              RETURN.
            ENDIF.

            IF lines( mt_modules_to_be_deleted ) > 0.

              IF mr_sdk_package_manager->sdk_zipfiles->ensure_zipfiles_downloaded( i_version = lv_target_version ) = abap_false.
                RETURN.
              ENDIF.

            ENDIF.

            l_jobnumber = mr_sdk_package_manager->job_manager->submit_batch_job( i_modules_to_be_installed = mt_modules_to_be_installed " empty since modules tbi are not collected here
                                                                                 i_modules_to_be_deleted   = mt_modules_to_be_deleted
                                                                                 i_target_version          = lv_target_version ).

            CONCATENATE `Submitted job with number ` l_jobnumber INTO l_job_message ##NO_TEXT.
            MESSAGE i000(0k) WITH l_job_message.

            refresh( ).

          WHEN 'UPD_INST'.

            toggle_inst_modules( i_checked = abap_true ).

          WHEN 'DES_INST'.

            toggle_inst_modules( i_checked = abap_false ).

          WHEN 'REF_TREE'.

            refresh( ).

          WHEN 'BTC_DTLS'.

            display_job_details( ).

          WHEN 'DOW_CERT'.

            mr_sdk_package_manager->certificate_manager->install_amazon_root_certs( ).

          WHEN 'DOW_TRAK'.

            mr_sdk_package_manager->sdk_zipfiles->download_zipfile_pair( i_version = 'LATEST' ).

          WHEN 'INS_ALL'.

            DATA(lv_text) = |Installing all modules is discouraged and should only be done in demo systems. |
                   && |Continue? | ##NO_TEXT.

            DATA: lv_answer TYPE string.

            CALL FUNCTION 'POPUP_TO_CONFIRM'
              EXPORTING
                titlebar              = ' '
                diagnose_object       = ' '
                text_question         = lv_text
                text_button_1         = |Do it anyway|
                icon_button_1         = ' '
                text_button_2         = |Go back|
                icon_button_2         = ' '
                default_button        = '2'
                display_cancel_button = ' '
                userdefined_f1_help   = ' '
                start_column          = 25
                start_row             = 6
                iv_quickinfo_button_1 = ' '
                iv_quickinfo_button_2 = ' '
              IMPORTING
                answer                = lv_answer
              EXCEPTIONS
                text_not_found        = 1
                OTHERS                = 2 ##NO_TEXT.
            IF sy-subrc <> 0.
              RAISE EXCEPTION TYPE lcx_error EXPORTING iv_msg = |Could not find text for popup| ##NO_TEXT.
            ENDIF.

            IF lv_answer = 1.

              l_jobnumber = mr_sdk_package_manager->install_all_modules( it_modules_to_be_installed = mt_modules_to_be_installed
                                                                         it_modules_to_be_deleted   = mt_modules_to_be_deleted ).

              CONCATENATE `Submitted job with number ` l_jobnumber INTO l_job_message ##NO_TEXT.
              MESSAGE i000(0k) WITH l_job_message.

              refresh( ).

            ELSE.
              RETURN.
            ENDIF.



          WHEN OTHERS.

            DATA l_message TYPE string.

            CONCATENATE 'Function code: ' e_salv_function INTO l_message ##NO_TEXT.
            MESSAGE i000(0k) WITH l_message.


        ENDCASE.
      CATCH lcx_error INTO DATA(lo_ex).
        lo_ex->show( ).
    ENDTRY.

  ENDMETHOD.


  METHOD handle_checkbox_changed.

    TRY.
        DATA(l_node) = mr_tree->get_nodes( )->get_node( node_key = node_key ).
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(l_tla) = l_node->get_text( ).
    DATA(wa_row) = CAST ts_sdk_module( l_node->get_data_row( ) )->*.

    IF l_tla = 'core'.

      CASE checked.

        WHEN abap_true.

          IF is_core_installed( ) = abap_false.
            wa_row-op_text = 'Will be installed.' ##NO_TEXT.
            wa_row-op_icon = '@08@'.
            wa_row-op_code = lif_ui_constants=>c_operation_install.
            l_node->set_data_row( wa_row ).

          ELSE.
            IF lcl_utils=>cmp_version_string( i_string1 = mt_installed_modules[ tla = 'sts' ]-avers
                                                          i_string2 = mt_installed_modules[ tla = 'sts' ]-cvers ) = 1.
              wa_row-op_text = 'Module will be updated.' ##NO_TEXT.
              wa_row-op_icon = '@09@'.
              wa_row-op_code = lif_ui_constants=>c_operation_update.
              l_node->set_data_row( wa_row ).
            ELSE.
              wa_row-op_text = 'Module up to date, no operation planned.' ##NO_TEXT.
              wa_row-op_icon = '@08@'.
              wa_row-op_code = lif_ui_constants=>c_operation_none.
              l_node->set_data_row( wa_row ).
            ENDIF.
          ENDIF.

        WHEN abap_false.

          IF is_core_installed( ) = abap_true.
            wa_row-op_text = 'Will be deleted.' ##NO_TEXT.
            wa_row-op_icon = '@0A@'.
            wa_row-op_code = lif_ui_constants=>c_operation_delete.
            l_node->set_data_row( wa_row ).


          ELSE.
            wa_row-op_text = 'Will NOT be installed.' ##NO_TEXT.
            wa_row-op_icon = '@EB@'.
            wa_row-op_code = lif_ui_constants=>c_operation_none.
            l_node->set_data_row( wa_row ).

          ENDIF.

      ENDCASE.

    ELSE.

      CASE checked.

        WHEN abap_true.

          IF VALUE #( mt_installed_modules[ tla = l_tla ] OPTIONAL ) IS INITIAL .
            wa_row-op_text = 'Will be installed.' ##NO_TEXT.
            wa_row-op_icon = '@08@'.
            wa_row-op_code = lif_ui_constants=>c_operation_install.
            l_node->set_data_row( wa_row ).

          ELSE.
            IF lcl_utils=>cmp_version_string( i_string1 = mt_installed_modules[ tla = l_tla ]-avers
                                                          i_string2 = mt_installed_modules[ tla = l_tla ]-cvers ) = 1.
              wa_row-op_text = 'Module will be updated.' ##NO_TEXT.
              wa_row-op_icon = '@09@' ##NO_TEXT.
              wa_row-op_code = lif_ui_constants=>c_operation_update.
              l_node->set_data_row( wa_row ).
            ELSEIF lcl_utils=>cmp_version_string( i_string1 = mt_installed_modules[ tla = l_tla ]-avers
                                                          i_string2 = mt_installed_modules[ tla = l_tla ]-cvers ) = 0.
              wa_row-op_text = 'Module up to date, no operation planned.' ##NO_TEXT.
              wa_row-op_icon = '@08@' ##NO_TEXT.
              wa_row-op_code = lif_ui_constants=>c_operation_none.
              l_node->set_data_row( wa_row ).
            ELSE.
              wa_row-op_text = 'Current version higher than available version. Module may be deprecated.' ##NO_TEXT.
              wa_row-op_icon = '@0A@' ##NO_TEXT.
              wa_row-op_code = lif_ui_constants=>c_operation_none.
              l_node->set_data_row( wa_row ).
            ENDIF.

          ENDIF.

        WHEN abap_false.

          IF VALUE #( mt_installed_modules[ tla = l_tla ] OPTIONAL ) IS NOT INITIAL.
            wa_row-op_text = 'Will be deleted.' ##NO_TEXT.
            wa_row-op_icon = '@0A@'.
            wa_row-op_code = lif_ui_constants=>c_operation_delete.
            l_node->set_data_row( wa_row ).

          ELSE.
            wa_row-op_text = 'Will NOT be installed.' ##NO_TEXT.
            wa_row-op_icon = '@EB@'.
            wa_row-op_code = lif_ui_constants=>c_operation_none.
            l_node->set_data_row( wa_row ).

          ENDIF.

      ENDCASE.

    ENDIF.

  ENDMETHOD.


  METHOD handle_double_click.

    TRY.
        DATA(l_node) = mr_tree->get_nodes( )->get_node( node_key = node_key ).
      CATCH cx_salv_msg.
    ENDTRY.

    DATA(l_tla) = l_node->get_text( ).
    DATA(wa_row) = CAST ts_sdk_module( l_node->get_data_row( ) )->*.

    IF ( columnname = 'TP_RC'
      OR columnname = 'TP_ICON'
      OR columnname = 'TP_TEXT' )
      AND ( wa_row-ctransport IS NOT INITIAL ).

      CALL FUNCTION 'TMS_UI_SHOW_TRANSPORT_LOGS'
        EXPORTING
          iv_request                 = CONV tmsbuffer-trkorr( wa_row-ctransport )
          iv_system                  = CONV tmscsys-sysnam( lcl_utils=>get_system_name( ) )
          iv_verbose                 = abap_true
        EXCEPTIONS
          show_transport_logs_failed = 1
          OTHERS                     = 2.
      IF sy-subrc <> 0.

      ENDIF.

    ENDIF.


    IF ( columnname = 'CVERS'
     OR columnname = 'CTRANSPORT' )
     AND ( wa_row-ctransport IS NOT INITIAL ).

      CALL FUNCTION 'TMS_UI_SHOW_TRANSPORT_REQUEST'
        EXPORTING
          iv_request                    = CONV tmsbuffer-trkorr( wa_row-ctransport )
          iv_target_system              = CONV tmscsys-sysnam( lcl_utils=>get_system_name( ) )
          iv_verbose                    = abap_true
        EXCEPTIONS
          show_transport_request_failed = 1
          OTHERS                        = 2.
      IF sy-subrc <> 0.

      ENDIF.

    ENDIF.


    IF ( columnname = 'NAME'
      OR columnname = 'AVERS'
      OR columnname = 'ATRANSPORT' ).

*      DATA: lr_dbox_container TYPE REF TO cl_gui_dialogbox_container.
*      DATA(lv_style) = cl_gui_control=>ws_thickframe + cl_gui_control=>ws_minimizebox + cl_gui_control=>ws_maximizebox + cl_gui_control=>ws_sysmenu.
*
*      lr_dbox_container = NEW cl_gui_dialogbox_container( caption = 'AWS SDK for SAP ABAP - API Documentation'
*                                                top     = 100
*                                                left    = 100
*                                                width   = 1750
*                                                height  = 1080
*                                                metric  = cl_gui_dialogbox_container=>metric_pixel ).
*
*      set handler handle_on_close for lr_dbox_container.
*
*      DATA: lr_html TYPE REF TO cl_gui_html_viewer.
*      lr_html = NEW cl_gui_html_viewer( parent = lr_dbox_container ).
*      lr_html->show_url( url      = |https://docs.aws.amazon.com/sdk-for-sap-abap/v1/api/latest/{ l_tla }/index.html|
*                         in_place = abap_true ).

      IF l_tla <> 'core'.
        cl_gui_frontend_services=>execute( document = |https://docs.aws.amazon.com/sdk-for-sap-abap/v1/api/latest/{ l_tla }/index.html| ).
      ELSE. " breakdown the line displaying 'core' into 4 distinct transports in a future commit
        cl_gui_frontend_services=>execute( document = |https://docs.aws.amazon.com/sdk-for-sap-abap/v1/api/latest/s3/index.html| ).
      ENDIF.


    ENDIF.


  ENDMETHOD.


  METHOD handle_on_close.

    IF sender IS NOT INITIAL.
      sender->free( ).
    ENDIF.

  ENDMETHOD.



  METHOD toggle_inst_modules.

    IF lines( mt_installed_modules ) > 0.

      TRY.
          DATA(lr_inst_mods_nodes) = mr_tree->get_nodes( )->get_node( node_key = mt_folder_node_keys[ node_name = 'installed_modules' ]-key )->get_children( ).

          LOOP AT lr_inst_mods_nodes INTO DATA(wa_inst_mod_node).

            DATA(lr_inst_mod_item) = wa_inst_mod_node-node->get_hierarchy_item( ).
            lr_inst_mod_item->set_checked( i_checked ).

            handle_checkbox_changed( node_key   = wa_inst_mod_node-node->get_key( )
                                     checked    = i_checked
                                     columnname = lr_inst_mod_item->get_columnname( ) ).

          ENDLOOP.
        CATCH cx_salv_msg.
      ENDTRY.

    ELSE.

      MESSAGE 'No modules to select!' TYPE 'E' DISPLAY LIKE 'E' ##NO_TEXT.

    ENDIF.

  ENDMETHOD.
ENDCLASS.


CLASS lcl_main DEFINITION FINAL.
  PUBLIC SECTION.
    METHODS
      main RAISING lcx_error.
ENDCLASS.

CLASS lcl_main IMPLEMENTATION.
  METHOD main.
    IF sy-batch = abap_true.
      DATA(sdk_pm_batch) = NEW lcl_sdk_package_manager( i_batch_mode = sy-batch ).
    ELSE.
      DATA(sdk_pm) = NEW lcl_sdk_package_manager( ).
      DATA(sdk_pm_tc) = NEW lcl_ui_tree_controller( ir_sdk_package_manager = sdk_pm ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.


START-OF-SELECTION.
  TRY.
      DATA(lr_main) = NEW lcl_main( ).
      lr_main->main( ).
    CATCH lcx_error INTO DATA(lo_ex).
      lo_ex->show( ).
  ENDTRY.
