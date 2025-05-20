class ZCL_ODATAV4_BASE_DPC definition
  public
  inheriting from /IWBEP/CL_V4_ABS_DATA_PROVIDER
  abstract
  create public .

public section.

  methods /IWBEP/IF_V4_DP_BASIC~CREATE_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~DELETE_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~READ_ENTITY
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~READ_ENTITY_LIST
    redefinition .
  methods /IWBEP/IF_V4_DP_BASIC~UPDATE_ENTITY
    redefinition .
protected section.

  methods GET_SERVICE_ID
  abstract
    returning
      value(RS_SERVICE_ID) type /IWBEP/SH_SERVICE_ID .
private section.
ENDCLASS.



CLASS ZCL_ODATAV4_BASE_DPC IMPLEMENTATION.


  method /IWBEP/IF_V4_DP_BASIC~CREATE_ENTITY.

    IF NOT zcl_odatav4_common_dpc=>create_from_service_id( get_service_id( ) )->create_entity(
             io_request  = io_request
             io_response = io_response ).

      super->/iwbep/if_v4_dp_basic~create_entity(
        EXPORTING
          io_request  = io_request
          io_response = io_response ).
    ENDIF.


  endmethod.


  method /IWBEP/IF_V4_DP_BASIC~DELETE_ENTITY.
    IF NOT zcl_odatav4_common_dpc=>create_from_service_id( get_service_id( ) )->delete_entity(
             io_request  = io_request
             io_response = io_response ).

      super->/iwbep/if_v4_dp_basic~delete_entity(
        EXPORTING
          io_request  = io_request
          io_response = io_response ).
    ENDIF.
  endmethod.


  METHOD /IWBEP/IF_V4_DP_BASIC~READ_ENTITY.

    IF NOT zcl_odatav4_common_dpc=>create_from_service_id( get_service_id( ) )->read_entity(
             io_request  = io_request
             io_response = io_response ).

      super->/iwbep/if_v4_dp_basic~read_entity(
        EXPORTING
          io_request  = io_request
          io_response = io_response ).
    ENDIF.

  ENDMETHOD.


  METHOD /IWBEP/IF_V4_DP_BASIC~READ_ENTITY_LIST.

    IF NOT zcl_odatav4_common_dpc=>create_from_service_id( get_service_id( ) )->read_entity_set(
             io_request  = io_request
             io_response = io_response ).

      super->/iwbep/if_v4_dp_basic~read_entity_list(
        EXPORTING
          io_request  = io_request
          io_response = io_response ).
    ENDIF.

  ENDMETHOD.


  METHOD /IWBEP/IF_V4_DP_BASIC~UPDATE_ENTITY.
    IF NOT zcl_odatav4_common_dpc=>create_from_service_id( get_service_id( ) )->update_entity(
             io_request  = io_request
             io_response = io_response ).

      super->/iwbep/if_v4_dp_basic~update_entity(
        EXPORTING
          io_request  = io_request
          io_response = io_response ).
    ENDIF.
  ENDMETHOD.
ENDCLASS.
