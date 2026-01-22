*"* use this source file for the definition and implementation of
*"* local helper classes, interface definitions and type
*"* declarations

INTERFACE lif_selected.

  METHODS stage_selected
    IMPORTING
      ii_event        TYPE REF TO zif_abapgit_gui_event
      it_status       TYPE zif_abapgit_definitions=>ty_results_ts_path
      it_local        TYPE zif_abapgit_definitions=>ty_files_item_tt
    RETURNING
      VALUE(ro_stage) TYPE REF TO zcl_abapgit_stage
    RAISING
      zcx_abapgit_exception.

ENDINTERFACE.

CLASS lcl_selected DEFINITION CREATE PRIVATE.

  PUBLIC SECTION.
    CLASS-METHODS get_instance
      RETURNING
        VALUE(ro_instance) TYPE REF TO lif_selected.

    INTERFACES lif_selected.

  PRIVATE SECTION.
    METHODS check_selected
      IMPORTING
        io_files TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    METHODS read_status_case_insensitive
      IMPORTING
        is_file          TYPE zif_abapgit_git_definitions=>ty_file
        it_status        TYPE zif_abapgit_definitions=>ty_results_ts_path
      RETURNING
        VALUE(rs_status) TYPE zif_abapgit_definitions=>ty_result
      RAISING
        zcx_abapgit_exception.

    METHODS parse_stage_data
      IMPORTING
        ii_event         TYPE REF TO zif_abapgit_gui_event
      RETURNING
        VALUE(ro_files)  TYPE REF TO zcl_abapgit_string_map
      RAISING
        zcx_abapgit_exception.

    CLASS-DATA:
      gi_instance TYPE REF TO lif_selected.

ENDCLASS.


CLASS lcl_selected IMPLEMENTATION.

  METHOD lif_selected~stage_selected.

    DATA ls_file  TYPE zif_abapgit_git_definitions=>ty_file.
    DATA ls_status LIKE LINE OF it_status.
    DATA lo_files TYPE REF TO zcl_abapgit_string_map.

    FIELD-SYMBOLS:
      <ls_file> LIKE LINE OF it_local,
      <ls_item> LIKE LINE OF lo_files->mt_entries.

    lo_files = parse_stage_data( ii_event ).

    IF lo_files->size( ) = 0.
      zcx_abapgit_exception=>raise( 'process_stage_list: empty list' ).
    ENDIF.

    check_selected( lo_files ).

    CREATE OBJECT ro_stage.

    LOOP AT lo_files->mt_entries ASSIGNING <ls_item>
      "Ignore Files that we don't want to stage, so any errors don't stop the staging process
      WHERE v <> zif_abapgit_definitions=>c_method-skip.

      zcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = <ls_item>-k
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      " you should remember that ls_file is sent from the client and is always uppercase,
      " whereas it_status and it_local path could be lower, upper or mixed case.
      ls_status = read_status_case_insensitive(
                      is_file   = ls_file
                      it_status = it_status ).

      " ls_status has the right case, therefore use it also for ls_file
      ls_file-path = ls_status-path.
      ls_file-filename = ls_status-filename.

      CASE <ls_item>-v.
        WHEN zif_abapgit_definitions=>c_method-add.
          READ TABLE it_local ASSIGNING <ls_file>
            WITH KEY file-path     = ls_file-path
                     file-filename = ls_file-filename.

          IF sy-subrc <> 0.
            zcx_abapgit_exception=>raise( |process_stage_list: unknown file { ls_file-path }{ ls_file-filename }| ).
          ENDIF.

          ro_stage->add( iv_path     = <ls_file>-file-path
                         iv_filename = <ls_file>-file-filename
                         is_status   = ls_status
                         iv_data     = <ls_file>-file-data ).
        WHEN zif_abapgit_definitions=>c_method-ignore.
          ro_stage->ignore( iv_path     = ls_file-path
                            iv_filename = ls_file-filename ).
        WHEN zif_abapgit_definitions=>c_method-rm.
          ro_stage->rm( iv_path     = ls_file-path
                        is_status   = ls_status
                        iv_filename = ls_file-filename ).
        WHEN zif_abapgit_definitions=>c_method-skip.
          " Do nothing. Never happens as it is filtered out before. Just for completeness.
        WHEN OTHERS.
          zcx_abapgit_exception=>raise( |process_stage_list: unknown method { <ls_item>-v }| ).
      ENDCASE.
    ENDLOOP.
  ENDMETHOD.


  METHOD check_selected.

    DATA:
      ls_file    TYPE zif_abapgit_git_definitions=>ty_file,
      lv_pattern TYPE string,
      lv_msg     TYPE string.

    FIELD-SYMBOLS:
      <ls_item>     LIKE LINE OF io_files->mt_entries,
      <ls_item_chk> LIKE LINE OF io_files->mt_entries.

    " Check all added files if the exist in different paths (packages) without being removed
    LOOP AT io_files->mt_entries ASSIGNING <ls_item> WHERE v = zif_abapgit_definitions=>c_method-add.

      " Allow mixed case path, but check filename to lower case
      zcl_abapgit_path=>split_file_location(
        EXPORTING
          iv_fullpath = <ls_item>-k
        IMPORTING
          ev_path     = ls_file-path
          ev_filename = ls_file-filename ).

      ls_file-filename = to_lower( ls_file-filename ).

      " Skip packages since they all have identical filenames
      IF NOT ls_file-filename CP 'package.devc.*'.
        lv_pattern = '*/' && to_upper( ls_file-filename ).
        REPLACE ALL OCCURRENCES OF '#' IN lv_pattern WITH '##'. " for CP

        LOOP AT io_files->mt_entries ASSIGNING <ls_item_chk>
          WHERE k CP lv_pattern AND k <> <ls_item>-k AND v <> zif_abapgit_definitions=>c_method-rm.

          lv_msg = |In order to add { to_lower( <ls_item>-k ) }, | &&
                   |you have to remove { to_lower( <ls_item_chk>-k ) }|.
          zcx_abapgit_exception=>raise( lv_msg ).

        ENDLOOP.
      ENDIF.
    ENDLOOP.

  ENDMETHOD.


  METHOD get_instance.

    IF gi_instance IS INITIAL.
      CREATE OBJECT gi_instance TYPE lcl_selected.
    ENDIF.

    ro_instance = gi_instance.

  ENDMETHOD.


  METHOD read_status_case_insensitive.

    FIELD-SYMBOLS: <ls_status> TYPE zif_abapgit_definitions=>ty_result.

    LOOP AT it_status ASSIGNING <ls_status>.

      IF to_upper( <ls_status>-filename ) = to_upper( is_file-filename )
      AND to_upper( <ls_status>-path )    = to_upper( is_file-path ).
        rs_status = <ls_status>.
        RETURN.
      ENDIF.

    ENDLOOP.

    " see https://github.com/abapGit/abapGit/issues/3073
    zcx_abapgit_exception=>raise(
      |Unable to stage { is_file-filename }. If the filename contains spaces, this is a known issue.| &&
      | Consider ignoring or staging the file at a later time.| ).

  ENDMETHOD.


  METHOD parse_stage_data.

    DATA lv_stage_data TYPE string.
    DATA lo_form_data TYPE REF TO zcl_abapgit_string_map.
    DATA lt_pairs TYPE string_table.
    DATA lv_pair TYPE string.
    DATA lv_key TYPE string.
    DATA lv_value TYPE string.
    DATA lv_colon_pos TYPE i.

    CREATE OBJECT ro_files.

    lo_form_data = ii_event->form_data( ).

    " Check if we have JSON-serialized stage data (new format for WebGUI compatibility)
    IF lo_form_data->has( 'stage_data' ).
      lv_stage_data = lo_form_data->get( 'stage_data' ).

      TRY.
          " Validate JSON format: should start with { and end with }
          lv_stage_data = condense( lv_stage_data ).
          IF strlen( lv_stage_data ) < 2 OR
             lv_stage_data(1) <> '{' OR
             substring( val = lv_stage_data
                        off = strlen( lv_stage_data ) - 1
                        len = 1 ) <> '}'.
            " Invalid JSON format, fall back to legacy
            ro_files = lo_form_data.
            RETURN.
          ENDIF.

          " Simple JSON parsing for flat object structure: {"key":"value","key2":"value2"}
          " This parser handles keys containing forward slashes, which are common in file paths
          " Expected values are single-character status codes:
          "   A = Add
          "   R = Remove
          "   I = Ignore
          "   ? = Reset
          " Remove braces
          lv_stage_data = replace( val = lv_stage_data
                                   sub = '{'
                                   with = '' ).
          lv_stage_data = replace( val = lv_stage_data
                                   sub = '}'
                                   with = '' ).

          " Split by comma (but be careful with commas inside quotes)
          " For simplicity, assume values don't contain commas or use a more robust parser
          " Since our values are just single characters (A, R, I), this should be safe

          " Parse each key-value pair
          DATA lv_in_quotes TYPE abap_bool VALUE abap_false.
          DATA lv_current_pair TYPE string.
          DATA lv_char TYPE c LENGTH 1.
          DATA lv_len TYPE i.
          DATA lv_idx TYPE i.

          lv_len = strlen( lv_stage_data ).
          lv_idx = 0.

          WHILE lv_idx < lv_len.
            lv_char = lv_stage_data+lv_idx(1).

            IF lv_char = '"'.
              IF lv_in_quotes = abap_false.
                lv_in_quotes = abap_true.
              ELSE.
                lv_in_quotes = abap_false.
              ENDIF.
            ELSEIF lv_char = ',' AND lv_in_quotes = abap_false.
              " End of a pair
              IF lv_current_pair IS NOT INITIAL.
                APPEND lv_current_pair TO lt_pairs.
                CLEAR lv_current_pair.
              ENDIF.
              lv_idx = lv_idx + 1.
              CONTINUE.
            ENDIF.

            lv_current_pair = lv_current_pair && lv_char.
            lv_idx = lv_idx + 1.
          ENDWHILE.

          " Don't forget the last pair
          IF lv_current_pair IS NOT INITIAL.
            APPEND lv_current_pair TO lt_pairs.
          ENDIF.

          " Now parse each pair to extract key and value
          LOOP AT lt_pairs INTO lv_pair.
            " Format should be "key":"value"
            " Find the colon between key and value
            FIND ':' IN lv_pair MATCH OFFSET lv_colon_pos.
            IF sy-subrc = 0.
              lv_key = substring( val = lv_pair
                                  len = lv_colon_pos ).
              lv_value = substring( val = lv_pair
                                    off = lv_colon_pos + 1 ).

              " Remove quotes and spaces
              REPLACE ALL OCCURRENCES OF '"' IN lv_key WITH ''.
              REPLACE ALL OCCURRENCES OF '"' IN lv_value WITH ''.
              CONDENSE lv_key NO-GAPS.
              CONDENSE lv_value NO-GAPS.

              IF lv_key IS NOT INITIAL AND lv_value IS NOT INITIAL.
                ro_files->set(
                  iv_key = lv_key
                  iv_val = lv_value ).
              ENDIF.
            ENDIF.
          ENDLOOP.

        CATCH cx_sy_range_out_of_bounds cx_sy_conversion_no_number.
          " If parsing fails (e.g., malformed JSON, unexpected format),
          " fall back to legacy format for backward compatibility
          ro_files = lo_form_data.
      ENDTRY.
    ELSE.
      " Legacy format: form data contains file paths directly as keys
      ro_files = lo_form_data.
    ENDIF.

  ENDMETHOD.

ENDCLASS.
