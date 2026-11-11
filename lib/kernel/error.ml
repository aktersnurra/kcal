open Base

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

let numeric_field_name = function
  | Calories_kcal -> "calories_kcal"
  | Protein_g -> "protein_g"
  | Carbs_g -> "carbs_g"
  | Fat_g -> "fat_g"
  | Confidence -> "confidence"
  | Weight_kg -> "weight_kg"
  | Calorie_target_kcal -> "calorie_target_kcal"
  | Protein_target_g -> "protein_target_g"

let field_name = function
  | Numeric field -> numeric_field_name field
  | Description -> "description"
  | Product_name -> "name"
  | Product_basis -> "basis"
  | Product_source -> "source"
  | External_id -> "external_id"

let to_string = function
  | Withings_failure failure -> (
      match failure with
      | Credential_rejection status ->
          Printf.sprintf "Withings authorization rejected (status %d)" status
      | Api_status status ->
          Printf.sprintf "Withings request failed (status %d)" status
      | Http_status status ->
          Printf.sprintf "Withings request failed (HTTP %d)" status
      | Malformed_response -> "malformed Withings response"
      | Transport_failure reason -> "Withings transport failed: " ^ reason)
  | Invalid_field { field; reason } ->
      let reason =
        match reason with
        | Blank -> "must not be blank"
        | Negative -> "must be nonnegative"
        | Nonpositive -> "must be positive"
        | Nonfinite -> "must be finite"
        | Out_of_range -> "must be between 0 and 1"
      in
      field_name field ^ " " ^ reason
  | Numeric_overflow field ->
      numeric_field_name field ^ " total exceeds the integer range"
  | Unrepresentable_result operation ->
      (match operation with
        | Daily_meals -> "daily meal totals"
        | Daily_weight -> "daily mean weight"
        | Average_intake -> "average intake"
        | Weight_regression -> "weight trend"
        | Implied_maintenance -> "implied maintenance")
      ^ " cannot be represented as a finite number"
  | Storage_decoding record -> "storage decoding error: invalid " ^ record
  | Unauthorized -> "unauthorized"
  | Not_found -> "not found"
  | Invalid_input reason -> "invalid input: " ^ reason
  | Conflict reason -> "conflict: " ^ reason
  | Storage_error reason -> "storage error: " ^ reason
  | Calendar_date_addition { date; days } ->
      Printf.sprintf
        "calendar limit: adding %d days to %s leaves years 0000-9999; choose a \
         date or day count within that calendar"
        days date
  | Calendar_day_bounds { date; timezone } ->
      Printf.sprintf
        "calendar limit: day %s in timezone %s requires a following date or \
         UTC bound outside years 0000-9999; choose a date away from the \
         calendar endpoint or another timezone"
        date timezone
  | Calendar_local_date { instant; utc_offset_s } ->
      Printf.sprintf
        "calendar limit: %s at UTC offset %d seconds has a local date outside \
         years 0000-9999; choose an instant away from the calendar endpoint or \
         another timezone"
        instant utc_offset_s
  | Invalid_date value ->
      Printf.sprintf "invalid date %S: expected YYYY-MM-DD" value
  | Unsupported_timezone value -> Printf.sprintf "unsupported timezone %S" value
  | Reversed_date_range { from_date; to_date } ->
      Printf.sprintf
        "from_date %s is after to_date %s (to_date defaults to today in the \
         requested timezone)"
        from_date to_date
  | Date_range_too_long { day_count; max_day_count } ->
      Printf.sprintf
        "date range covers %d days; at most %d are allowed, so split it into \
         several calls"
        day_count max_day_count
  | Nonpositive_argument name -> Printf.sprintf "%s must be positive" name
  | Rejected reason -> reason
