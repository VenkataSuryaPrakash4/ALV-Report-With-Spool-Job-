*&---------------------------------------------------------------------*
*& Include          ZRFI_CUST_OPEN_DETAILS_CD
*&---------------------------------------------------------------------*
*&---------------------------------------------------------------------*
*& Form screen_validation
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM screen_validation .
  LOOP AT SCREEN INTO wa_screen.
    IF wa_screen-group1 = 'BRC' AND
       rb_rad1 = abap_true.
      wa_screen-active = 0.
    ENDIF.
    MODIFY SCREEN FROM wa_screen.
    CLEAR:wa_screen.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form get_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM get_data .
  SELECT rbukrs,gjahr,belnr,tsl,budat,bldat,kunnr
    FROM acdoca
    INTO TABLE @gt_acdoca
    WHERE rbukrs = @p_bukrs AND
          gjahr = @p_gjahr AND
          belnr IN @s_belnr AND
          koart = 'D'.
  IF sy-subrc = 0.
    SORT gt_acdoca BY rbukrs kunnr budat.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form process_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM process_data .
  DATA:lv_date     TYPE sy-datum,
       lv_ext_date TYPE char10.

  LOOP AT gt_acdoca INTO DATA(lwa_acdoca).
    gv_bukrs = lwa_acdoca-rbukrs.
    gv_kunnr = lwa_acdoca-kunnr.
    gv_budat = lwa_acdoca-budat.

    CALL FUNCTION 'BAPI_AR_ACC_GETOPENITEMS'
      EXPORTING
        companycode = gv_bukrs
        customer    = gv_kunnr
        keydate     = gv_budat
        noteditems  = 'X'
        secindex    = 'X'
      TABLES
        lineitems   = gt_items.

    READ TABLE gt_items INTO DATA(lwa_items) WITH KEY  comp_code = lwa_acdoca-rbukrs
                                                 fisc_year = lwa_acdoca-gjahr
                                                 doc_no = lwa_acdoca-belnr
                                                 customer = lwa_acdoca-kunnr.
    IF sy-subrc = 0.
      wa_final-bukrs = lwa_acdoca-rbukrs.
      wa_final-gjahr = lwa_acdoca-gjahr.
      wa_final-belnr = lwa_acdoca-belnr.
      wa_final-budat = lwa_acdoca-budat.
      lv_date = wa_final-budat.
      CLEAR: wa_final-budat.

      PERFORM convert_date_ext USING lv_date
                               CHANGING lv_ext_date.

      wa_final-budat = lv_ext_date.
      CLEAR:lv_date,lv_ext_date.

      wa_final-bldat = lwa_acdoca-bldat.
      lv_date = wa_final-bldat.
      CLEAR: wa_final-bldat.

      PERFORM convert_date_ext USING lv_date
                             CHANGING lv_ext_date.
      wa_final-bldat = lv_ext_date.
      CLEAR:lv_date,lv_ext_date.

      wa_final-dmbtr = lwa_acdoca-tsl.
      IF wa_final-dmbtr LT 0.
        wa_final-icon = 1.
      ELSEIF wa_final-dmbtr = 0.
        wa_final-icon = 2.
      ELSE.
        wa_final-icon = 3.
      ENDIF.

      IF rb_rad1 = abap_true.
        COLLECT wa_final INTO gt_final.
      ELSE. "logic for detailed
        APPEND wa_final TO gt_final.
      ENDIF.
    ENDIF.
    CLEAR:lwa_acdoca,gv_bukrs,gv_kunnr,gv_budat,wa_final,lwa_items.
    REFRESH:gt_items.
  ENDLOOP.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form display_data
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM display_data .

  IF rb_rad1 = abap_true.
    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program = sy-repid
        i_structure_name   = 'ZSTTST'
        i_grid_title       = 'Open Items'
      TABLES
        t_outtab           = gt_final
      EXCEPTIONS
        program_error      = 1
        OTHERS             = 2.
  ELSE.
*---Field catalog
    ADD 1 TO gv_count.
*    APPEND VALUE #( col_pos = gv_count fieldname = 'ICON'
*                    key = 'X' outputlen = 3
*                    seltext_s = 'LIGHT' ) TO gt_fieldcat.

    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BUKRS'
                    key = 'X' outputlen = 5
                    seltext_s = 'Comp Code' ) TO gt_fieldcat.
    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'GJAHR'
                    key = 'X' outputlen = 5
                    seltext_s = 'Fisc Year' ) TO gt_fieldcat.

    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BELNR'
                    key = 'X' outputlen = 10
                    seltext_s = 'Doc num'
                    no_zero = 'X') TO gt_fieldcat.

    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BUDAT'
                    outputlen = 10
                    seltext_s = 'Post Date' ) TO gt_fieldcat.

    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'BLDAT'
                    outputlen = 10
                    seltext_s = 'Doc Date' ) TO gt_fieldcat.

    ADD 1 TO gv_count.
    APPEND VALUE #( col_pos = gv_count fieldname = 'DMBTR'
                    do_sum = 'X' outputlen = 15
                    seltext_s = 'Amount'
                    edit = 'X' ) TO gt_fieldcat.

*--- Layout
    wa_layout-lights_fieldname = 'ICON'.
    wa_layout-zebra = 'X'.
    wa_layout-colwidth_optimize = 'X'.

*--- Sort
    APPEND VALUE #( fieldname = 'BELNR'
                    tabname = 'LT_FINAL' up = 'X'
                    subtot = 'X' ) TO gt_sort.

*--- Filter
    gt_filter = VALUE #( ( fieldname = 'BELNR' tabname = 'LT_FINAL'
                         sign0 = 'I' optio = 'BT'
                         valuf_int = '0000001406'
                         valut_int = '0000001803' ) ).

    CALL FUNCTION 'REUSE_ALV_GRID_DISPLAY'
      EXPORTING
        i_callback_program       = sy-repid
        i_callback_pf_status_set = 'SET_PF_STATUS'
        i_callback_user_command  = 'USR_CMND'
*       i_callback_top_of_page   = 'TOP_OF_PAGE'
        is_layout                = wa_layout
        it_fieldcat              = gt_fieldcat
        it_sort                  = gt_sort
        it_filter                = gt_filter
        i_save                   = 'A'
*       it_events                = ' '
      TABLES
        t_outtab                 = gt_final
      EXCEPTIONS
        program_error            = 1
        OTHERS                   = 2.
    IF sy-subrc <> 0.
* Implement suitable error handling here
    ENDIF.
  ENDIF.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form convert_Date_ext
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*&      --> LV_DATE
*&      <-- WA_FINAL_BUDAT
*&      <-- WA_FINAL_BLDAT
*&---------------------------------------------------------------------*
FORM convert_date_ext  USING f_lv_date
                       CHANGING f_lv_ext_date.

  CALL FUNCTION 'CONVERT_DATE_TO_EXTERNAL'
    EXPORTING
      date_internal            = f_lv_date
    IMPORTING
      date_external            = f_lv_ext_date
    EXCEPTIONS
      date_internal_is_invalid = 1
      OTHERS                   = 2.


ENDFORM.

FORM top_of_page.
  DATA:lt_head TYPE slis_t_listheader.
  DATA(lv_inp) = |{ p_bukrs }| & |/| & |{ p_gjahr }|.
  lt_head = VALUE #( ( typ = 'H' info = 'Customer ageing report' )
                     ( typ = 'S' key = 'Date' info = sy-datum )
                     ( typ = 'S' key = 'User' info = sy-uname )
                     ( typ = 'S' key = 'Inputs' info = lv_inp ) ).

  CALL FUNCTION 'REUSE_ALV_COMMENTARY_WRITE'
    EXPORTING
      it_list_commentary = lt_head
      i_logo             = 'SAPABAP_IMG'
*     I_END_OF_LIST_GRID =
*     I_ALV_FORM         =
    .

ENDFORM.
*&---------------------------------------------------------------------*
*& Form clear
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM clear .
  CLEAR:gt_acdoca.
ENDFORM.
*&---------------------------------------------------------------------*
*& Form inputs
*&---------------------------------------------------------------------*
*& text
*&---------------------------------------------------------------------*
*& -->  p1        text
*& <--  p2        text
*&---------------------------------------------------------------------*
FORM inputs .
  DATA:lv_txt TYPE char100.
  IF p_bukrs IS NOT INITIAL.
    SELECT SINGLE * FROM t001 INTO @DATA(wa_t001) WHERE bukrs = @p_bukrs.
    IF sy-subrc NE 0.
      lv_txt = TEXT-005.
      REPLACE ALL OCCURRENCES OF '&' IN lv_txt WITH p_bukrs.
      MESSAGE lv_txt TYPE 'E'.
    ENDIF.
  ENDIF.
ENDFORM.

FORM set_pf_status USING fcode TYPE slis_t_extab.
  SET PF-STATUS 'ZSTANDARD'.
ENDFORM.

FORM usr_cmnd USING action TYPE sy-ucomm
                   rs_selfield type slis_selfield.
*              tables     rs_selfield TYPE slis_selfield.
  DATA: lv_jobcnt  TYPE tbtcjob-jobcount, "job number
        lv_jobname TYPE tbtcjob-jobname. " job name
  CASE action.
    WHEN '&DWN'.

      lv_jobname = 'ZRFI_CUST_OPEN_DETAILS'.
      CALL FUNCTION 'JOB_OPEN' "open a job
        EXPORTING
          jobname          = lv_jobname
        IMPORTING
          jobcount         = lv_jobcnt
        EXCEPTIONS
          cant_create_job  = 1
          invalid_job_data = 2
          jobname_missing  = 3
          OTHERS           = 4.
      DATA : lv_rqdest TYPE tsp01-rqdest VALUE 'LP01',
             lv_linsz  TYPE sylinsz VALUE '9999999',
             lt_param  TYPE rsparams_tt,
             lv_auth   TYPE tbtcjob-authcknam.


*---- Get print parameters
DATA:lwa_params type pri_params.
    call function 'GET_PRINT_PARAMETERS'
      exporting
        destination    = 'LP01'
        immediately    = ' '
        line_count     = '65'
        line_size      = '255'
        no_dialog      = 'X'
        cover_page     = ''
      importing
        out_parameters = lwa_params.

      lv_auth = sy-uname.

      CALL FUNCTION 'JOB_SUBMIT'
        EXPORTING
*         ARCPARAMS               =
          authcknam               = lv_auth
*         COMMANDNAME             = ' '
*         OPERATINGSYSTEM         = ' '
*         EXTPGM_NAME             = ' '
*         EXTPGM_PARAM            = ' '
*         EXTPGM_SET_TRACE_ON     = ' '
*         EXTPGM_STDERR_IN_JOBLOG = 'X'
*         EXTPGM_STDOUT_IN_JOBLOG = 'X'
*         EXTPGM_SYSTEM           = ' '
*         EXTPGM_RFCDEST          = ' '
*         EXTPGM_WAIT_FOR_TERMINATION       = 'X'
          jobcount                = lv_jobcnt
          jobname                 = lv_jobname
          language                = sy-langu
          PRIPARAMS               = lwa_params
          report                  = sy-repid
*         VARIANT                 = ' '
*       IMPORTING
*         STEP_NUMBER             =
        EXCEPTIONS
          bad_priparams           = 1
          bad_xpgflags            = 2
          invalid_jobdata         = 3
          jobname_missing         = 4
          job_notex               = 5
          job_submit_failed       = 6
          lock_failed             = 7
          program_missing         = 8
          prog_abap_and_extpg_set = 9
          OTHERS                  = 10.
      IF sy-subrc <> 0.
* Implement suitable error handling here
      ENDIF.

*      SUBMIT zrfi_cust_open_details
* TO SAP-SPOOL DESTINATION lv_rqdest
* LINE-SIZE lv_linsz
* IMMEDIATELY 'X'
* KEEP IN SPOOL 'X'
* USER sy-uname VIA JOB lv_jobname NUMBER lv_jobcnt
* WITHOUT SPOOL DYNPRO
* WITH SELECTION-TABLE lt_param
* AND RETURN.

      CALL FUNCTION 'JOB_CLOSE' "job close
        EXPORTING
          jobcount             = lv_jobcnt
          jobname              = lv_jobname
          strtimmed            = 'X'
        EXCEPTIONS
          cant_start_immediate = 1
          invalid_startdate    = 2
          jobname_missing      = 3
          job_close_failed     = 4
          job_nosteps          = 5
          job_notex            = 6
          lock_failed          = 7
          invalid_target       = 8
          OTHERS               = 9.

      DATA: lv_job TYPE tbtcv-fin.
      DO 120 TIMES.
        CALL FUNCTION 'BDL_READ_JOB_STATUS' "get job status
          EXPORTING
            jobname       = lv_jobname
            jobnumber     = lv_jobcnt
          IMPORTING
            jobstatus     = lv_job
          EXCEPTIONS
            job_not_found = 1
            OTHERS        = 2.
        IF lv_job NE 'F'. "job finished
          WAIT UP TO 1 SECONDS.
          CONTINUE.
        ELSE.
          EXIT.
        ENDIF.
      ENDDO.

      DATA(lv_leng) = strlen( sy-repid ).
      IF lv_leng GE 9.
        DATA(lv_rname) = |{ sy-repid+0(9) }| & |{ sy-uname+0(3) }|.
      ELSE.
        DATA(lv_strl) = 9 - lv_leng.
        DO lv_strl TIMES.
          lv_rname = |{ sy-repid }| & |_|.
        ENDDO.
        lv_rname = |{ sy-repid }| & |{ sy-uname+0(3) }|.
      ENDIF.
      CONDENSE lv_rname NO-GAPS.

      DATA:lv_rq2name TYPE tsp01-rq2name.
      CONSTANTS:c_slash TYPE c VALUE '\'.

      lv_rq2name = lv_rname.
      SELECT rqident,rqcretime
             FROM tsp01
             INTO TABLE @DATA(lt_spool)
             WHERE rq2name = @lv_rq2name.
      IF sy-subrc = 0.
        SORT lt_spool BY rqcretime DESCENDING.
        DATA(lwa_spool) = lt_spool[ 1 ] .
      ENDIF.
      DATA:lv_path TYPE string.
      CALL METHOD cl_gui_frontend_services=>get_desktop_directory
        CHANGING
          desktop_directory    = lv_path
        EXCEPTIONS
          cntl_error           = 1
          error_no_gui         = 2
          not_supported_by_gui = 3
          OTHERS               = 4.
      IF sy-subrc = 0.
        CALL METHOD cl_gui_cfw=>update_view.
        lv_path = |{ lv_path }|  & |{ c_slash }| & | open_det.pdf |.
      ENDIF.

      DATA:ls_param TYPE rsparams.

      ls_param-selname = 'SPOOLNO'.
      ls_param-sign = 'I'.
      ls_param-option = 'EQ'.
      ls_param-low = lwa_spool-rqident.
      ls_param-high = ' '.
      APPEND ls_param TO lt_param.

      ls_param-selname = 'P_FILE'.
      ls_param-sign = 'I'.
      ls_param-option = 'EQ'.
      ls_param-low = lv_path.
      ls_param-high = ' '.
      APPEND ls_param TO lt_param.

      SUBMIT rstxpdft4 WITH SELECTION-TABLE lt_param.
    WHEN '&SAVE'.
      BREAK-POINT.
      rs_selfield-refresh = 'X'.
  ENDCASE.
ENDFORM.
