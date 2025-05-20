class ZCL_ODATAV4_COMMON_DPC definition
  public
  final
  create private .

public section.

  data MV_GROUP_ID type /IWFND/V4_MED_GROUP_ID read-only .
  data MS_SERVICE_ID type /IWBEP/SH_SERVICE_ID read-only .

  methods READ_ENTITY_SET
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_LIST
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_LIST
    returning
      value(RV_CUSTOMIZING_FOUND) type ABAP_BOOL
    raising
      /IWBEP/CX_GATEWAY .
  methods DELETE_ENTITY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_DELETE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_DELETE
    returning
      value(RV_CUSTOMIZING_FOUND) type ABAP_BOOL
    raising
      /IWBEP/CX_GATEWAY .
  methods UPDATE_ENTITY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_UPDATE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_UPDATE
    returning
      value(RV_CUSTOMIZING_FOUND) type ABAP_BOOL
    raising
      /IWBEP/CX_GATEWAY .
  methods CREATE_ENTITY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_CREATE
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_CREATE
    returning
      value(RV_CUSTOMIZING_FOUND) type ABAP_BOOL
    raising
      /IWBEP/CX_GATEWAY .
  methods READ_ENTITY
    importing
      !IO_REQUEST type ref to /IWBEP/IF_V4_REQU_BASIC_READ
      !IO_RESPONSE type ref to /IWBEP/IF_V4_RESP_BASIC_READ
    returning
      value(RV_CUSTOMIZING_FOUND) type ABAP_BOOL
    raising
      /IWBEP/CX_GATEWAY .
  class-methods CREATE_FROM_SERVICE_ID
    importing
      !IS_SERVICE_ID type /IWBEP/SH_SERVICE_ID
    returning
      value(RO_INSTANCE) type ref to ZCL_ODATAV4_COMMON_DPC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ODATAV4_COMMON_DPC IMPLEMENTATION.


  METHOD create_entity.
    DATA:
      lv_entity_name             TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
      lr_structure               TYPE REF TO data,
      ls_todo_list               TYPE /iwbep/if_v4_requ_basic_create=>ty_s_todo_list,
      ls_done_list               TYPE /iwbep/if_v4_requ_basic_create=>ty_s_todo_process_list,
      lv_key_string              TYPE string,
      lt_registered_entity_list  TYPE zcl_odatav4_common_mpc=>stt_entities.

    FIELD-SYMBOLS:
      <ls_data>         TYPE any,
      <lv_key_value>    TYPE any.

    zcl_odatav4_common_mpc=>create_from_service_id( ms_service_id )->get_entity_list(
      IMPORTING
        et_entities = lt_registered_entity_list ).

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entity_name ).

    ASSIGN lt_registered_entity_list[ entity_internal_name = lv_entity_name ] TO FIELD-SYMBOL(<ls_registered_entity>).
    IF sy-subrc EQ 0 AND <ls_registered_entity>-createable EQ abap_on.
      rv_customizing_found = abap_true.

      "Check not CDS
      SELECT SINGLE ddlname
        FROM ddddlsrc
        WHERE ddlname EQ @<ls_registered_entity>-view_name
        INTO @DATA(lv_ddl_name).
      IF sy-subrc EQ 0.
        IF 1 = 2.
          MESSAGE s048(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 048
                                           attr1 = <ls_registered_entity>-view_name )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
      ENDIF.

      CREATE DATA lr_structure TYPE (<ls_registered_entity>-view_name).
      ASSIGN lr_structure->* TO <ls_data>.

      io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
      IF ls_todo_list-process-busi_data = abap_true.
        io_request->get_busi_data( IMPORTING es_busi_data = <ls_data> ).
        ls_done_list-busi_data = abap_true.
      ENDIF.

      CLEAR lv_key_string.
      LOOP AT <ls_registered_entity>-key_list ASSIGNING FIELD-SYMBOL(<lv_key_name>).
        ASSIGN COMPONENT <lv_key_name> OF STRUCTURE <ls_data> TO <lv_key_value>.
        IF lv_key_string IS NOT INITIAL.
          lv_key_string = |{ lv_key_string } AND|.
        ENDIF.
        lv_key_string = |{ lv_key_string } { <lv_key_name> } = '{ <lv_key_value> }'|.
      ENDLOOP.

      SELECT COUNT( * )
        FROM (<ls_registered_entity>-view_name)
        WHERE (lv_key_string).
      IF sy-subrc eq 0.
        IF 1 = 2.
          MESSAGE s047(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 047
                                           attr1 = lv_key_string )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-conflict.

      ENDIF.

      MODIFY (<ls_registered_entity>-view_name)
        FROM <ls_data>.

      IF sy-subrc EQ 0.
        io_response->set_busi_data( is_busi_data = <ls_data> ).
      ELSE.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = sy-msgid
                                           msgno = sy-msgno
                                           attr1 = sy-msgv1
                                           attr2 = sy-msgv2
                                           attr3 = sy-msgv3
                                           attr4 = sy-msgv4 )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
      ENDIF.

      io_response->set_is_done( ls_done_list ).

    ELSE.
      rv_customizing_found = abap_false.
    ENDIF.

  ENDMETHOD.


  method CREATE_FROM_SERVICE_ID.
    ro_instance = NEW #( ).
    ro_instance->ms_service_id = is_service_id.
  endmethod.


  METHOD DELETE_ENTITY.
    DATA:
      lv_entity_name             TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
      lr_structure               TYPE REF TO data,
      lr_key                     TYPE REF TO data,
      ls_todo_list               TYPE /iwbep/if_v4_requ_basic_delete=>ty_s_todo_list,
      ls_done_list               TYPE /iwbep/if_v4_requ_basic_delete=>ty_s_todo_process_list,
      lv_key_string              TYPE string,
      lt_registered_entity_list  TYPE zcl_odatav4_common_mpc=>stt_entities.

    FIELD-SYMBOLS:
      <ls_data>               TYPE any,
      <ls_key_data>           TYPE any,
      <lv_key_value>          TYPE any,
      <lv_key_value_payload>  TYPE any.

    zcl_odatav4_common_mpc=>create_from_service_id( ms_service_id )->get_entity_list(
      IMPORTING
        et_entities = lt_registered_entity_list ).

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entity_name ).

    ASSIGN lt_registered_entity_list[ entity_internal_name = lv_entity_name ] TO FIELD-SYMBOL(<ls_registered_entity>).
    IF sy-subrc EQ 0 AND <ls_registered_entity>-deletable EQ abap_on.
      rv_customizing_found = abap_true.

      "Check not CDS
      SELECT SINGLE ddlname
        FROM ddddlsrc
        WHERE ddlname EQ @<ls_registered_entity>-view_name
        INTO @DATA(lv_ddl_name).
      IF sy-subrc EQ 0.
        IF 1 = 2.
          MESSAGE s048(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 048
                                           attr1 = <ls_registered_entity>-view_name )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
      ENDIF.

      CREATE DATA lr_key TYPE (<ls_registered_entity>-view_name).
      ASSIGN lr_key->* TO <ls_key_data>.

      io_request->get_key_data( IMPORTING es_key_data = <ls_key_data> ).
      ls_done_list-key_data = abap_true.

      CLEAR lv_key_string.
      LOOP AT <ls_registered_entity>-key_list ASSIGNING FIELD-SYMBOL(<lv_key_name>).
        ASSIGN COMPONENT <lv_key_name> OF STRUCTURE <ls_key_data> TO <lv_key_value>.
        IF lv_key_string IS NOT INITIAL.
          lv_key_string = |{ lv_key_string } AND|.
        ENDIF.
        lv_key_string = |{ lv_key_string } { <lv_key_name> } = '{ <lv_key_value> }'|.
      ENDLOOP.

      SELECT COUNT( * )
        FROM (<ls_registered_entity>-view_name)
        WHERE (lv_key_string).
      IF sy-subrc ne 0.
        IF 1 = 2.
          MESSAGE s050(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 050
                                           attr1 = lv_key_string )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-not_found.

      ENDIF.

      DELETE FROM (<ls_registered_entity>-view_name)
        WHERE (lv_key_string).

      IF sy-subrc NE 0.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = sy-msgid
                                           msgno = sy-msgno
                                           attr1 = sy-msgv1
                                           attr2 = sy-msgv2
                                           attr3 = sy-msgv3
                                           attr4 = sy-msgv4 )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
      ENDIF.

      io_response->set_is_done( ls_done_list ).

    ELSE.
      rv_customizing_found = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD read_entity.
    DATA:
      lv_entity_name             TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
      lr_structure               TYPE REF TO data,
      ls_done_list               TYPE /iwbep/if_v4_requ_basic_read=>ty_s_todo_process_list,
      lv_key_string              TYPE string,
      lt_registered_entity_list  TYPE zcl_odatav4_common_mpc=>stt_entities.

    FIELD-SYMBOLS:
      <ls_data>         TYPE any,
      <lv_key_value>    TYPE any.

    zcl_odatav4_common_mpc=>create_from_service_id( ms_service_id )->get_entity_list(
      IMPORTING
        et_entities = lt_registered_entity_list ).

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entity_name ).

    ASSIGN lt_registered_entity_list[ entity_internal_name = lv_entity_name ] TO FIELD-SYMBOL(<ls_registered_entity>).
    IF sy-subrc EQ 0.
      rv_customizing_found = abap_true.

      CREATE DATA lr_structure TYPE (<ls_registered_entity>-view_name).
      ASSIGN lr_structure->* TO <ls_data>.

      io_request->get_key_data( IMPORTING es_key_data = <ls_data> ).
      ls_done_list-key_data = abap_true.

      CLEAR lv_key_string.
      LOOP AT <ls_registered_entity>-key_list ASSIGNING FIELD-SYMBOL(<lv_key_name>).
        ASSIGN COMPONENT <lv_key_name> OF STRUCTURE <ls_data> TO <lv_key_value>.
        IF lv_key_string IS NOT INITIAL.
          lv_key_string = |{ lv_key_string } AND|.
        ENDIF.
        lv_key_string = |{ lv_key_string } { <lv_key_name> } = '{ <lv_key_value> }'|.
      ENDLOOP.

      SELECT SINGLE *
        FROM (<ls_registered_entity>-view_name)
        INTO @<ls_data>
        WHERE (lv_key_string).

      IF sy-subrc EQ 0.
        io_response->set_busi_data( is_busi_data = <ls_data> ).
      ELSE.
        IF 1 = 2.
          MESSAGE s050(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 050
                                           attr1 = lv_key_string )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-not_found.
      ENDIF.

      io_response->set_is_done( ls_done_list ).

    ELSE.
      rv_customizing_found = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD read_entity_set.

    DATA:
      lv_entityset_name    TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
      ls_todo_list         TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_list,
      ls_done_list         TYPE /iwbep/if_v4_requ_basic_list=>ty_s_todo_process_list,
      lv_where_clause      TYPE string,
      lv_select_string     TYPE string,
      lv_orderby_string    TYPE string,
      lt_selected_property TYPE /iwbep/if_v4_runtime_types=>ty_t_property_path,
      lt_orderby_property  TYPE abap_sortorder_tab,
      lv_skip              TYPE i VALUE 0,
      lv_top               TYPE i VALUE 0,
      lv_count             TYPE i,
      lv_max_index         TYPE i,
      lr_target_table      TYPE REF TO data,
      lv_key_string_full   TYPE string,
      lv_key_string        TYPE string,
      lt_registered_entity_list  TYPE zcl_odatav4_common_mpc=>stt_entities.

    FIELD-SYMBOLS:
      <lt_data>            TYPE STANDARD TABLE,
      <ls_data>            TYPE any,
      <lv_key_value>       TYPE any.

    io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
    io_request->get_entity_set( IMPORTING ev_entity_set_name = lv_entityset_name ).

    zcl_odatav4_common_mpc=>create_from_service_id( ms_service_id )->get_entity_list(
      IMPORTING
        et_entities = lt_registered_entity_list ).

    "Sorting
    IF ls_todo_list-process-orderby = abap_true.
      ls_done_list-orderby = abap_true.

      io_request->get_orderby( IMPORTING et_orderby_property = lt_orderby_property ).
      CLEAR lv_orderby_string.
      LOOP AT lt_orderby_property INTO DATA(ls_orderby_property).
        IF ls_orderby_property-descending = abap_true.
          CONCATENATE lv_orderby_string ls_orderby_property-name 'DESCENDING' INTO lv_orderby_string SEPARATED BY space.
        ELSE.
          CONCATENATE lv_orderby_string ls_orderby_property-name 'ASCENDING' INTO lv_orderby_string SEPARATED BY space.
        ENDIF.
      ENDLOOP.
    ELSE.
      lv_orderby_string = 'PRIMARY KEY'.
    ENDIF.

    "Field list
    IF ls_todo_list-process-select = abap_true.
      ls_done_list-select = abap_true.
      io_request->get_selected_properties(  IMPORTING et_selected_property = lt_selected_property ).
      CONCATENATE LINES OF lt_selected_property INTO lv_select_string  SEPARATED BY ','.
    ELSE.
      lv_select_string = '*'.
    ENDIF.

    "Filter
    IF ls_todo_list-process-filter = abap_true.
      ls_done_list-filter = abap_true.
      io_request->get_filter_osql_where_clause( IMPORTING ev_osql_where_clause = lv_where_clause ).
    ENDIF.

    "Pagination
    IF ls_todo_list-process-skip = abap_true.
      ls_done_list-skip = abap_true.
      io_request->get_skip( IMPORTING ev_skip = lv_skip ).
    ENDIF.
    IF ls_todo_list-process-top = abap_true.
      ls_done_list-top = abap_true.
      io_request->get_top( IMPORTING ev_top = lv_top ).
    ELSE.
      lv_top = 10.
    ENDIF.
    lv_max_index = lv_top + lv_skip.

    ASSIGN lt_registered_entity_list[ entityset_internal_name = lv_entityset_name ] TO FIELD-SYMBOL(<ls_registered_entity>).
    IF sy-subrc EQ 0.
      rv_customizing_found = abap_true.

      CREATE DATA lr_target_table TYPE TABLE OF (<ls_registered_entity>-view_name).
      ASSIGN lr_target_table->* TO <lt_data>.

      "Key list
      io_request->get_key_data( IMPORTING et_key_data = <lt_data> ).
      ls_done_list-key_data = abap_true.

      CLEAR lv_key_string_full.
      LOOP AT <lt_data> ASSIGNING <ls_data>.
        IF lv_key_string_full IS NOT INITIAL.
          lv_key_string_full = |{ lv_key_string_full } OR|.
        ENDIF.
        CLEAR lv_key_string.
        LOOP AT <ls_registered_entity>-key_list ASSIGNING FIELD-SYMBOL(<lv_key_name>).
          ASSIGN COMPONENT <lv_key_name> OF STRUCTURE <ls_data> TO <lv_key_value>.
          IF lv_key_string IS NOT INITIAL.
            lv_key_string = |{ lv_key_string } AND|.
          ENDIF.
          lv_key_string = |{ lv_key_string } { <lv_key_name> } = '{ <lv_key_value> }'|.
        ENDLOOP.
        IF lv_key_string IS NOT INITIAL.
          lv_key_string_full = |{ lv_key_string_full } ( { lv_key_string } )|.
        ENDIF.
      ENDLOOP.

      "Data selection
      IF ls_todo_list-return-busi_data = abap_true.

        SELECT (lv_select_string)
          FROM (<ls_registered_entity>-view_name)
          WHERE ( (lv_where_clause) )
            AND ( (lv_key_string_full) )
          ORDER BY (lv_orderby_string)
          INTO CORRESPONDING FIELDS OF TABLE @<lt_data>
          UP TO @lv_max_index ROWS.

        IF lv_skip IS NOT INITIAL.
          DELETE <lt_data> TO lv_skip.
        ENDIF.

        io_response->set_busi_data( it_busi_data = <lt_data> ).
      ELSE.
        IF ls_todo_list-return-count = abap_true.

          SELECT COUNT( * )
            FROM (<ls_registered_entity>-view_name)
            WHERE ( (lv_where_clause) )
              AND ( (lv_key_string_full) )
            INTO @lv_count.

          io_response->set_count( lv_count ).
        ENDIF.
      ENDIF.

      io_response->set_is_done( ls_done_list ).
    ELSE.
      rv_customizing_found = abap_false.
    ENDIF.

  ENDMETHOD.


  METHOD UPDATE_ENTITY.
    DATA:
      lv_entity_name             TYPE /iwbep/if_v4_med_element=>ty_e_med_internal_name,
      lr_structure               TYPE REF TO data,
      lr_key                     TYPE REF TO data,
      ls_todo_list               TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_list,
      ls_done_list               TYPE /iwbep/if_v4_requ_basic_update=>ty_s_todo_process_list,
      lv_key_string              TYPE string,
      lt_registered_entity_list  TYPE zcl_odatav4_common_mpc=>stt_entities.

    FIELD-SYMBOLS:
      <ls_data>               TYPE any,
      <ls_key_data>           TYPE any,
      <lv_key_value>          TYPE any,
      <lv_key_value_payload>  TYPE any.

    zcl_odatav4_common_mpc=>create_from_service_id( ms_service_id )->get_entity_list(
      IMPORTING
        et_entities = lt_registered_entity_list ).

    io_request->get_entity_type( IMPORTING ev_entity_type_name = lv_entity_name ).

    ASSIGN lt_registered_entity_list[ entity_internal_name = lv_entity_name ] TO FIELD-SYMBOL(<ls_registered_entity>).
    IF sy-subrc EQ 0 AND <ls_registered_entity>-updateable EQ abap_on.
      rv_customizing_found = abap_true.

      "Check not CDS
      SELECT SINGLE ddlname
        FROM ddddlsrc
        WHERE ddlname EQ @<ls_registered_entity>-view_name
        INTO @DATA(lv_ddl_name).
      IF sy-subrc EQ 0.
        IF 1 = 2.
          MESSAGE s048(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 048
                                           attr1 = <ls_registered_entity>-view_name )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
      ENDIF.

      CREATE DATA lr_structure TYPE (<ls_registered_entity>-view_name).
      ASSIGN lr_structure->* TO <ls_data>.
      CREATE DATA lr_key TYPE (<ls_registered_entity>-view_name).
      ASSIGN lr_key->* TO <ls_key_data>.

      io_request->get_todos( IMPORTING es_todo_list = ls_todo_list ).
      IF ls_todo_list-process-busi_data = abap_true.
        io_request->get_busi_data( IMPORTING es_busi_data = <ls_data> ).
        ls_done_list-busi_data = abap_true.
      ENDIF.

      io_request->get_key_data( IMPORTING es_key_data = <ls_key_data> ).
      ls_done_list-key_data = abap_true.

      CLEAR lv_key_string.
      LOOP AT <ls_registered_entity>-key_list ASSIGNING FIELD-SYMBOL(<lv_key_name>).
        ASSIGN COMPONENT <lv_key_name> OF STRUCTURE <ls_key_data> TO <lv_key_value>.
        ASSIGN COMPONENT <lv_key_name> OF STRUCTURE <ls_data> TO <lv_key_value_payload>.
        IF <lv_key_value_payload> IS INITIAL.
          <lv_key_value_payload> = <lv_key_value>.
        ELSEIF <lv_key_value_payload> IS NOT INITIAL AND <lv_key_value_payload> NE <lv_key_value>.
          IF 1 = 2.
            MESSAGE s046(zotc_odata_w2).
          ENDIF.
          RAISE EXCEPTION TYPE /iwbep/cx_gateway
            EXPORTING
              textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                             msgno = 046
                                             attr1 = <lv_key_name>
                                             attr2 = <lv_key_value>
                                             attr3 = <lv_key_value_payload> )
              http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
        ENDIF.
        IF lv_key_string IS NOT INITIAL.
          lv_key_string = |{ lv_key_string } AND|.
        ENDIF.
        lv_key_string = |{ lv_key_string } { <lv_key_name> } = '{ <lv_key_value> }'|.
      ENDLOOP.

      SELECT COUNT( * )
        FROM (<ls_registered_entity>-view_name)
        WHERE (lv_key_string).
      IF sy-subrc ne 0.
        IF 1 = 2.
          MESSAGE s050(zotc_odata_w2).
        ENDIF.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = 'ZOTC_ODATA_W2'
                                           msgno = 050
                                           attr1 = lv_key_string )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-not_found.

      ENDIF.

      MODIFY (<ls_registered_entity>-view_name)
        FROM <ls_data>.

      IF sy-subrc EQ 0.
        io_response->set_busi_data( is_busi_data = <ls_data> ).
      ELSE.
        RAISE EXCEPTION TYPE /iwbep/cx_gateway
          EXPORTING
            textid              = VALUE #( msgid = sy-msgid
                                           msgno = sy-msgno
                                           attr1 = sy-msgv1
                                           attr2 = sy-msgv2
                                           attr3 = sy-msgv3
                                           attr4 = sy-msgv4 )
            http_status_code    = /iwbep/cx_gateway=>gcs_http_status_codes-sv_internal_server_error.
      ENDIF.

      io_response->set_is_done( ls_done_list ).

    ELSE.
      rv_customizing_found = abap_false.
    ENDIF.

  ENDMETHOD.
ENDCLASS.
