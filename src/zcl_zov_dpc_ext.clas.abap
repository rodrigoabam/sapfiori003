class ZCL_ZOV_DPC_EXT definition
  public
  inheriting from ZCL_ZOV_DPC
  create public .

public section.
protected section.

  methods HEADER_OVSET_CREATE_ENTITY
    redefinition .
  methods HEADER_OVSET_DELETE_ENTITY
    redefinition .
  methods HEADER_OVSET_GET_ENTITYSET
    redefinition .
  methods HEADER_OVSET_UPDATE_ENTITY
    redefinition .
  methods MENSAGEMSET_CREATE_ENTITY
    redefinition .
  methods MENSAGEMSET_DELETE_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITY
    redefinition .
  methods MENSAGEMSET_GET_ENTITYSET
    redefinition .
  methods MENSAGEMSET_UPDATE_ENTITY
    redefinition .
  methods HEADER_OVSET_GET_ENTITY
    redefinition .
private section.
ENDCLASS.



CLASS ZCL_ZOV_DPC_EXT IMPLEMENTATION.


  method HEADER_OVSET_CREATE_ENTITY.
  endmethod.


  method HEADER_OVSET_DELETE_ENTITY.
  endmethod.


  method HEADER_OVSET_GET_ENTITY.
  endmethod.


  method HEADER_OVSET_GET_ENTITYSET.
  endmethod.


  method HEADER_OVSET_UPDATE_ENTITY.
  endmethod.


  method MENSAGEMSET_CREATE_ENTITY.
  endmethod.


  method MENSAGEMSET_DELETE_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITY.
  endmethod.


  method MENSAGEMSET_GET_ENTITYSET.
  endmethod.


  method MENSAGEMSET_UPDATE_ENTITY.
  endmethod.
ENDCLASS.
