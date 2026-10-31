type invalid_field_reason =
  | Blank
  | Negative
  | Nonpositive
  | Nonfinite
  | Out_of_range

type numeric_field =
  | Calories_kcal
  | Protein_g
  | Carbs_g
  | Fat_g
  | Confidence
  | Weight_kg
  | Calorie_target_kcal
  | Protein_target_g

type field =
  | Numeric of numeric_field
  | Description
  | Product_name
  | Product_basis
  | Product_source
  | External_id

type calculation =
  | Daily_meals
  | Daily_weight
  | Average_intake
  | Weight_regression
  | Implied_maintenance

type provider_failure =
  | Credential_rejection of int
  | Api_status of int
  | Http_status of int
  | Malformed_response
  | Transport_failure of string

type t =
  | Withings_failure of provider_failure
  | Invalid_field of { field : field; reason : invalid_field_reason }
  | Numeric_overflow of numeric_field
  | Unrepresentable_result of calculation
  | Storage_decoding of string
  | Unauthorized
  | Not_found
  | Invalid_input of string
  | Conflict of string
  | Storage_error of string
  | Calendar_date_addition of { date : string; days : int }
  | Calendar_day_bounds of { date : string; timezone : string }
  | Calendar_local_date of { instant : string; utc_offset_s : int }
  | Invalid_date of string
  | Unsupported_timezone of string
  | Reversed_date_range of { from_date : string; to_date : string }
  | Date_range_too_long of { day_count : int; max_day_count : int }
  | Nonpositive_argument of string
  | Rejected of string
      (** A well-formed request the ledger cannot honour; the message says what
          to change. *)

val to_string : t -> string
