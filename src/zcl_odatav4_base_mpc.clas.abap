class ZCL_ODATAV4_BASE_MPC definition
  public
  inheriting from /IWBEP/CL_V4_ABS_MODEL_PROV
  abstract
  create public .

public section.

  methods /IWBEP/IF_V4_MP_BASIC~DEFINE
    redefinition .
protected section.

  methods GET_SERVICE_ID
  abstract
    returning
      value(RS_SERVICE_ID) type /IWBEP/SH_SERVICE_ID .
private section.
ENDCLASS.



CLASS ZCL_ODATAV4_BASE_MPC IMPLEMENTATION.


  METHOD /IWBEP/IF_V4_MP_BASIC~DEFINE.

    zcl_odatav4_common_mpc=>create_from_service_id( get_service_id( ) )->/iwbep/if_v4_mp_basic~define(
      EXPORTING
        io_model      = io_model
        io_model_info = io_model_info ).

  ENDMETHOD.
ENDCLASS.
