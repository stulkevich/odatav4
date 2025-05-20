class ZCL_ODATAV4_COMMON_MPC definition
  public
  final
  create private .

public section.

  interfaces /IWBEP/IF_V4_MP_BASIC .

  types:
    BEGIN OF sts_entity.
      INCLUDE TYPE zodatav4_entity.
      TYPES: key_list     TYPE /iwbep/if_v4_med_types=>ty_t_med_internal_name ,
    END OF sts_entity .
  types:
    stt_entities TYPE TABLE OF sts_entity .

  data MV_GROUP_ID type /IWFND/V4_MED_GROUP_ID read-only .
  data MS_SERVICE_ID type /IWBEP/SH_SERVICE_ID read-only .

  class-methods DEFINE_MODEL_BY_DDIC_STRUCTURE
    importing
      !IV_STRUCTURE_NAME type /IWBEP/IF_V4_MED_TYPES=>TY_E_MED_INTERNAL_NAME
      !IT_KEYS type /IWBEP/IF_V4_MED_TYPES=>TY_T_MED_INTERNAL_NAME
      !IS_ENTITY type /IWBEP/IF_V4_MED_TYPES=>TY_S_MED_ENTITY_TYPE
      !IS_ENTITY_SET type /IWBEP/IF_V4_MED_TYPES=>TY_S_MED_ENTITY_TYPE optional
      !IO_MODEL type ref to /IWBEP/IF_V4_MED_MODEL
    exporting
      !EO_ENTITY type ref to /IWBEP/IF_V4_MED_ENTITY_TYPE
      !EO_ENTITY_SET type ref to /IWBEP/IF_V4_MED_ENTITY_SET
    raising
      /IWBEP/CX_GATEWAY .
  methods GET_ENTITY_LIST
    exporting
      !ET_ENTITIES type STT_ENTITIES .
  class-methods ADD_NAVIGATION_PROPERTIES
    importing
      !IO_ENTITY type ref to /IWBEP/IF_V4_MED_ENTITY_TYPE
      !IO_ENTITY_SET type ref to /IWBEP/IF_V4_MED_ENTITY_SET
      !IV_STRUCTURE_NAME type /IWBEP/IF_V4_MED_TYPES=>TY_E_MED_INTERNAL_NAME
      !IS_SERVICE_ID type /IWBEP/SH_SERVICE_ID
    raising
      /IWBEP/CX_GATEWAY .
  class-methods CREATE_FROM_SERVICE_ID
    importing
      !IS_SERVICE_ID type /IWBEP/SH_SERVICE_ID
    returning
      value(RO_INSTANCE) type ref to ZCL_ODATAV4_COMMON_MPC .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ODATAV4_COMMON_MPC IMPLEMENTATION.


  METHOD /iwbep/if_v4_mp_basic~define.
    DATA:
      lt_entity_list          TYPE stt_entities.

    get_entity_list(
      IMPORTING
        et_entities = lt_entity_list ).

    LOOP AT lt_entity_list ASSIGNING FIELD-SYMBOL(<ls_entity>).
      define_model_by_ddic_structure(
        EXPORTING
          iv_structure_name = <ls_entity>-view_name
          it_keys           = <ls_entity>-key_list
          is_entity         = VALUE #( internal_name = <ls_entity>-entity_internal_name     edm_name = <ls_entity>-entity_edm_name )
          is_entity_set     = VALUE #( internal_name = <ls_entity>-entityset_internal_name  edm_name = <ls_entity>-entityset_edm_name )
          io_model          = io_model ).

    ENDLOOP.

    "Navigation Properties
    LOOP AT lt_entity_list ASSIGNING <ls_entity>.
      DATA(lo_entity)     = io_model->get_entity_type( <ls_entity>-entity_internal_name ).
      IF <ls_entity>-entityset_internal_name IS NOT INITIAL.
        DATA(lo_entity_set) = io_model->get_entity_set( <ls_entity>-entityset_internal_name ).
      ENDIF.

      add_navigation_properties(
        EXPORTING
          io_entity         = lo_entity
          io_entity_set     = lo_entity_set
          is_service_id     = ms_service_id
          iv_structure_name = <ls_entity>-view_name ).
    ENDLOOP.

  ENDMETHOD.


  METHOD add_navigation_properties.
    "Navigation properties
    SELECT property_internal_name, property_edm_name,
           target_entity_internal,  multiplicity,
           delete_cascade, entityset_internal_name
      FROM zodatav4_nprop
      JOIN zodatav4_entity ON (
        zodatav4_entity~service_id      EQ zodatav4_nprop~service_id AND
        zodatav4_entity~service_version EQ zodatav4_nprop~service_version AND
        zodatav4_entity~entity_internal_name EQ zodatav4_nprop~target_entity_internal )
      WHERE zodatav4_nprop~service_id      EQ @is_service_id-service_id
        AND zodatav4_nprop~service_version EQ @is_service_id-service_version
        AND zodatav4_nprop~view_name EQ @iv_structure_name
      INTO TABLE @DATA(lt_nav_properties).

    LOOP AT lt_nav_properties ASSIGNING FIELD-SYMBOL(<ls_nav_property>).
      DATA(lo_nav_property) = io_entity->create_navigation_property( <ls_nav_property>-property_internal_name ).
      lo_nav_property->set_target_entity_type_name( <ls_nav_property>-target_entity_internal ).
      lo_nav_property->set_target_multiplicity( <ls_nav_property>-multiplicity ).
      lo_nav_property->set_edm_name( <ls_nav_property>-property_edm_name ).
      IF <ls_nav_property>-delete_cascade EQ abap_on.
        lo_nav_property->set_on_delete_action( /iwbep/if_v4_med_element=>gcs_med_on_delete_action-cascade ).
      ENDIF.

      IF io_entity_set IS BOUND.
        io_entity_set->add_navigation_prop_binding(
          iv_navigation_property_path = CONV #( <ls_nav_property>-property_internal_name )
          iv_target_entity_set        = <ls_nav_property>-entityset_internal_name ).
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD create_from_service_id.
    ro_instance = NEW #( ).
    ro_instance->ms_service_id = is_service_id.
  ENDMETHOD.


  METHOD define_model_by_ddic_structure.
    DATA:
      lo_entity               TYPE REF TO /iwbep/if_v4_med_entity_type,
      lo_entity_set           TYPE REF TO /iwbep/if_v4_med_entity_set,
      lt_primitive_properties TYPE /iwbep/if_v4_med_element=>ty_t_med_prim_property,
      lr_structure            TYPE REF TO data.
    FIELD-SYMBOLS:
      <ls_data>               TYPE any.

    CREATE DATA lr_structure TYPE (iv_structure_name).
    ASSIGN lr_structure->* TO <ls_data>.

    lo_entity =
      io_model->create_entity_type_by_struct(
        EXPORTING
          iv_entity_type_name          = is_entity-internal_name
          is_structure                 = <ls_data>
          iv_add_conv_to_prim_props    = abap_true
          iv_add_f4_help_to_prim_props = abap_true
          iv_gen_prim_props            = abap_true ).

    lo_entity->set_edm_name( is_entity-edm_name ).

    lo_entity->get_primitive_properties( IMPORTING et_property = lt_primitive_properties ).

    LOOP AT it_keys ASSIGNING FIELD-SYMBOL(<lv_key_name>).
      DATA(lo_primitive_property) = lo_entity->get_primitive_property( <lv_key_name> ).
      lo_primitive_property->set_is_key( ).
    ENDLOOP.

    LOOP AT lt_primitive_properties INTO lo_primitive_property.
      lo_primitive_property->set_edm_name( to_mixed( val = lo_primitive_property->get_internal_name( ) ) ).
      TRY.
        lo_primitive_property->set_is_nullable( ).
      CATCH /iwbep/cx_v4_med.
        "Is Key
      ENDTRY.
    ENDLOOP.

    IF is_entity_set IS SUPPLIED.
      lo_entity_set = lo_entity->create_entity_set( is_entity_set-internal_name ).
      lo_entity_set->set_edm_name( is_entity_set-edm_name ).
    ENDIF.
  ENDMETHOD.


  METHOD get_entity_list.
    SELECT *
      FROM zodatav4_entity
      WHERE service_id      EQ @ms_service_id-service_id
        AND service_version EQ @ms_service_id-service_version
      INTO CORRESPONDING FIELDS OF TABLE @et_entities.

    LOOP AT et_entities ASSIGNING FIELD-SYMBOL(<ls_entity>).
      SPLIT <ls_entity>-keys AT ',' INTO TABLE <ls_entity>-key_list.
      LOOP AT <ls_entity>-key_list ASSIGNING FIELD-SYMBOL(<lv_key>).
        <lv_key> = to_upper( condense( <lv_key> ) ).
      ENDLOOP.
    ENDLOOP.

  ENDMETHOD.
ENDCLASS.
