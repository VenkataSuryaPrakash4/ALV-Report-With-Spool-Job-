*&---------------------------------------------------------------------*
*& Include          ZRFI_CUST_OPEN_DETAILS_GD
*&---------------------------------------------------------------------*
TYPE-POOLS:slis.
TABLES:acdoca.

TYPES:BEGIN OF ty_final,
        ICON TYPE c,
        bukrs TYPE bukrs,
        gjahr TYPE gjahr,
        belnr TYPE belnr_d,
        budat TYPE budat,
        dmbtr TYPE dmbtr,
      END OF ty_final,

      BEGIN OF ty_acdoca,
        rbukrs TYPE bukrs,
        gjahr  TYPE gjahr,
        belnr  TYPE belnr_d,
        tsl    TYPE fins_vtcur12,
        budat  TYPE budat,
        bldat  type bldat,
        kunnr  TYPE kunnr,
      END OF ty_acdoca.

DATA:wa_screen    TYPE screen,
     gt_acdoca    TYPE TABLE OF ty_acdoca,
     gt_items     TYPE TABLE OF bapi3007_2,
     gt_items_det TYPE TABLE OF bapi3007_2,
     gt_fieldcat  TYPE slis_t_fieldcat_alv,
     gt_sort      TYPE slis_t_sortinfo_alv,
     gt_filter TYPE slis_t_filter_alv,

     wa_layout    TYPE slis_layout_alv,

     gv_bukrs     TYPE bapi3007_1-comp_code,
     gv_kunnr     TYPE bapi3007_1-customer,
     gv_budat     TYPE bapi3007-key_date,
     gt_final     TYPE TABLE OF zsttst,
     wa_final     TYPE zsttst,
     gv_count     TYPE n LENGTH 2.
