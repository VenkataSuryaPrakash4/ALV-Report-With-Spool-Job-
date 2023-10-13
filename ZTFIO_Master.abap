*&---------------------------------------------------------------------*
*& Report ZRFI_CUST_OPEN_DETAILS
*&---------------------------------------------------------------------*
*&
*&---------------------------------------------------------------------*
REPORT ztfio.

INCLUDE ZRFI_CUST_OPEN_DETAILS_GD1.
*INCLUDE zrfi_cust_open_details_gd.  "Global Declarations
INCLUDE ZRFI_CUST_OPEN_DETAILS_SS1.
*INCLUDE zrfi_cust_open_details_ss.  "Selection Screen
INCLUDE ZRFI_CUST_OPEN_DETAILS_CD1.
*INCLUDE zrfi_cust_open_details_cd.  "Business Logic

INITIALIZATION.
PERFORM clear.

AT SELECTION-SCREEN OUTPUT.
  PERFORM screen_validation.

AT SELECTION-SCREEN.
PERFORM inputs.

START-OF-SELECTION.
  PERFORM get_data.
  PERFORM process_data.
  PERFORM display_data.
