class ZCL_ZOV_MPC_EXT definition
  public
  inheriting from ZCL_ZOV_MPC
  create public .

public section.

  types:
    BEGIN OF ty_order_item
      , OrderID     TYPE I
      , DataCriacao TYPE TIMESTAMP
      , CriadoPor   TYPE C length 20
      , ClientId   TYPE I
      , TotalItens  TYPE P length 8 decimals 2
      , TotalFrete  TYPE P length 8 decimals 2
      , TotalOrder  TYPE P length 8 decimals 2
      , Status      TYPE C length 1
      , toOVItem    TYPE TABLE OF ts_ov_item WITH DEFAULT KEY
      , END OF ty_order_item .

  methods DEFINE
    redefinition .
protected section.
private section.
ENDCLASS.



CLASS ZCL_ZOV_MPC_EXT IMPLEMENTATION.


method DEFINE.
  DATA lo_entity_type TYPE REF TO /iwbep/if_mgw_odata_entity_typ.

  super->define( ).

  lo_entity_type = model->get_entity_type( iv_entity_name = 'HEADER_OV' ).
  lo_entity_type->bind_structure( iv_structure_name = 'ZCL_ZOV_MPC_EXT=>TY_ORDER_ITEM' ).
endmethod.
ENDCLASS.
