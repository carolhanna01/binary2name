#include <rpc/xdr.h>


struct dis_float_vector {
	float x;
	float y;
	float z;
};
typedef struct dis_float_vector dis_float_vector;
bool_t xdr_dis_float_vector (XDR *, dis_float_vector *);


struct dis_angular_vel_vector {
	float x;
	float y;
	float z;
};
typedef struct dis_angular_vel_vector dis_angular_vel_vector;
bool_t xdr_dis_angular_vel_vector (XDR *, dis_angular_vel_vector *);


struct dis_linear_acc_vector {
	float x;
	float y;
	float z;
};
typedef struct dis_linear_acc_vector dis_linear_acc_vector;
bool_t xdr_dis_linear_acc_vector (XDR *, dis_linear_acc_vector *);


struct dis_linear_vel_vector {
	float x;
	float y;
	float z;
};
typedef struct dis_linear_vel_vector dis_linear_vel_vector;
bool_t xdr_dis_linear_vel_vector (XDR *, dis_linear_vel_vector *);


struct dis_entity_coord_vector {
	float x;
	float y;
	float z;
};
typedef struct dis_entity_coord_vector dis_entity_coord_vector;
bool_t xdr_dis_entity_coord_vector (XDR *, dis_entity_coord_vector *);


struct dis_entity_type {
	u_char kind;
	u_char domain;
	u_short country;
	u_char category;
	u_char subcategory;
	u_char specific;
	u_char extra;
};
typedef struct dis_entity_type dis_entity_type;
bool_t xdr_dis_entity_type (XDR *, dis_entity_type *);


struct dis_entity_marking {
	u_char charset;
	u_char marking[11];
};
typedef struct dis_entity_marking dis_entity_marking;
bool_t xdr_dis_entity_marking (XDR *, dis_entity_marking *);


struct dis_fixed_datum {
	u_long datum_id;
	u_long ulong_value;
};
typedef struct dis_fixed_datum dis_fixed_datum;
bool_t xdr_dis_fixed_datum (XDR *, dis_fixed_datum *);

typedef struct {
 unsigned long datum_id;
 unsigned long value_length;
 union {
 double double_value;
 dis_entity_type entity_type_value;
 unsigned char *ptr_value;
 } value;
 } dis_variable_datum;

typedef struct {
 unsigned time:31; /* 1.676... usec units */
 unsigned type:1; /* type: 0=relative, 1=absolute */
 } dis_timestamp;

struct dis_simulation_addr {
	u_short site_id;
	u_short application_id;
};
typedef struct dis_simulation_addr dis_simulation_addr;
bool_t xdr_dis_simulation_addr (XDR *, dis_simulation_addr *);


struct dis_emitter_system {
	u_short name;
	u_char function;
	u_char id;
};
typedef struct dis_emitter_system dis_emitter_system;
bool_t xdr_dis_emitter_system (XDR *, dis_emitter_system *);


struct dis_entity_id {
	dis_simulation_addr sim_id;
	u_short entity_id;
};
typedef struct dis_entity_id dis_entity_id;
bool_t xdr_dis_entity_id (XDR *, dis_entity_id *);


struct dis_euler_angles {
	float psi;
	float theta;
	float phi;
};
typedef struct dis_euler_angles dis_euler_angles;
bool_t xdr_dis_euler_angles (XDR *, dis_euler_angles *);


struct dis_event_id {
	dis_simulation_addr sim_id;
	u_short event_id;
};
typedef struct dis_event_id dis_event_id;
bool_t xdr_dis_event_id (XDR *, dis_event_id *);


struct dis_fundamental_parameters {
	float freq;
	float freq_range;
	float erp;
	float prf;
	float pulse_width;
	float beam_azimuth_center;
	float beam_azimuth_sweep;
	float beam_elev_center;
	float beam_elev_sweep;
	float beam_sweep_sync;
};
typedef struct dis_fundamental_parameters dis_fundamental_parameters;
bool_t xdr_dis_fundamental_parameters (XDR *, dis_fundamental_parameters *);


struct dis_modulation_type {
	u_short spread_spectrum;
	u_short major_type;
	u_short detail;
	u_short system;
};
typedef struct dis_modulation_type dis_modulation_type;
bool_t xdr_dis_modulation_type (XDR *, dis_modulation_type *);


struct dis_pdu_header {
	u_char protocol_version;
	u_char exercise_id;
	u_char pdu_type;
	u_char protocol_family;
	dis_timestamp time_stamp;
	u_short length;
	u_short padding;
};
typedef struct dis_pdu_header dis_pdu_header;
bool_t xdr_dis_pdu_header (XDR *, dis_pdu_header *);


struct dis_double_vector {
	double x;
	double y;
	double z;
};
typedef struct dis_double_vector dis_double_vector;
bool_t xdr_dis_double_vector (XDR *, dis_double_vector *);


struct dis_world_coordinates {
	double x;
	double y;
	double z;
};
typedef struct dis_world_coordinates dis_world_coordinates;
bool_t xdr_dis_world_coordinates (XDR *, dis_world_coordinates *);


struct dis_relative_coordinates {
	float x;
	float y;
	float z;
};
typedef struct dis_relative_coordinates dis_relative_coordinates;
bool_t xdr_dis_relative_coordinates (XDR *, dis_relative_coordinates *);


struct dis_antenna_location {
	dis_world_coordinates ant_location;
	dis_relative_coordinates relative;
};
typedef struct dis_antenna_location dis_antenna_location;
bool_t xdr_dis_antenna_location (XDR *, dis_antenna_location *);


struct dis_beam_antenna_pattern {
	dis_euler_angles direction;
	float azimuth_width;
	float elev_width;
	u_char reference_system;
	u_char pad[3];
	float Ez;
	float Ex;
	float phase;
};
typedef struct dis_beam_antenna_pattern dis_beam_antenna_pattern;
bool_t xdr_dis_beam_antenna_pattern (XDR *, dis_beam_antenna_pattern *);


struct dis_spherical_harmonic_antenna_pattern {
	u_char pattern;
	float coefficients;
	u_char ref_system;
};
typedef struct dis_spherical_harmonic_antenna_pattern dis_spherical_harmonic_antenna_pattern;
bool_t xdr_dis_spherical_harmonic_antenna_pattern (XDR *, dis_spherical_harmonic_antenna_pattern *);

typedef union {
 double d;
 float f[2];
 char c[8];
 long l[2];
 short s[4];
 } dis_parm_value;

typedef struct {
 unsigned char type;
 unsigned char change;
 unsigned short id;
 unsigned long parm_type;
 dis_parm_value value;
 } dis_articulation_parm;

struct dis_time {
	u_long hour;
	dis_timestamp rel;
};
typedef struct dis_time dis_time;
bool_t xdr_dis_time (XDR *, dis_time *);


struct dis_burst_descriptor {
	dis_entity_type munition;
	u_short warhead;
	u_short fuze;
	u_short quantity;
	u_short rate;
};
typedef struct dis_burst_descriptor dis_burst_descriptor;
bool_t xdr_dis_burst_descriptor (XDR *, dis_burst_descriptor *);


struct dis_dead_reckoning {
	u_char algorithm;
	u_char other[15];
	dis_linear_acc_vector linear_acc;
	dis_angular_vel_vector angular_vel;
};
typedef struct dis_dead_reckoning dis_dead_reckoning;
bool_t xdr_dis_dead_reckoning (XDR *, dis_dead_reckoning *);


typedef u_long dis_capabilities;
bool_t xdr_dis_capabilities (XDR *, dis_capabilities *);


struct dis_supply_quantity {
	dis_entity_type entity;
	float quantity;
};
typedef struct dis_supply_quantity dis_supply_quantity;
bool_t xdr_dis_supply_quantity (XDR *, dis_supply_quantity *);


typedef u_long dis_entity_appearance;
bool_t xdr_dis_entity_appearance (XDR *, dis_entity_appearance *);


struct dis_entity_state_pdu {
	dis_pdu_header hdr;
	dis_entity_id id;
	u_char force_id;
	u_char art_parm_count;
	dis_entity_type type;
	dis_entity_type alt_type;
	dis_linear_vel_vector vel;
	dis_world_coordinates pos;
	dis_euler_angles orientation;
	dis_entity_appearance appearance;
	dis_dead_reckoning dr_parm;
	dis_entity_marking marking;
	dis_capabilities capabilities;
	dis_articulation_parm *art_parm;
};
typedef struct dis_entity_state_pdu dis_entity_state_pdu;
bool_t xdr_dis_entity_state_pdu (XDR *, dis_entity_state_pdu *);


struct dis_collision_pdu {
	dis_pdu_header hdr;
	dis_entity_id id;
	dis_entity_id collision_id;
	dis_event_id event;
	dis_linear_vel_vector vel;
	u_long mass;
	dis_entity_coord_vector loc;
};
typedef struct dis_collision_pdu dis_collision_pdu;
bool_t xdr_dis_collision_pdu (XDR *, dis_collision_pdu *);


struct dis_fire_pdu {
	dis_pdu_header hdr;
	dis_entity_id firing_id;
	dis_entity_id target_id;
	dis_entity_id munition_id;
	dis_event_id event;
	u_long fire_mission_index;
	dis_world_coordinates pos;
	dis_burst_descriptor burst;
	dis_linear_vel_vector vel;
	float range;
};
typedef struct dis_fire_pdu dis_fire_pdu;
bool_t xdr_dis_fire_pdu (XDR *, dis_fire_pdu *);


struct dis_detonation_pdu {
	dis_pdu_header hdr;
	dis_entity_id firing_id;
	dis_entity_id target_id;
	dis_entity_id munition_id;
	dis_event_id event;
	dis_linear_vel_vector vel;
	dis_world_coordinates pos;
	dis_burst_descriptor burst;
	dis_entity_coord_vector loc;
	u_char result;
	u_char num_art_parms;
	u_short pad;
	dis_articulation_parm *art_parm;
};
typedef struct dis_detonation_pdu dis_detonation_pdu;
bool_t xdr_dis_detonation_pdu (XDR *, dis_detonation_pdu *);


typedef u_char dis_service_type;
bool_t xdr_dis_service_type (XDR *, dis_service_type *);


typedef u_short dis_repair_type;
bool_t xdr_dis_repair_type (XDR *, dis_repair_type *);


typedef u_char dis_repair_result;
bool_t xdr_dis_repair_result (XDR *, dis_repair_result *);


struct dis_service_request_pdu {
	dis_pdu_header hdr;
	dis_entity_id requestor_id;
	dis_entity_id server_id;
	dis_service_type requested_service;
	u_char num_supply_types;
	u_short pad;
	dis_supply_quantity *supplies;
};
typedef struct dis_service_request_pdu dis_service_request_pdu;
bool_t xdr_dis_service_request_pdu (XDR *, dis_service_request_pdu *);


struct dis_resupply_offer_pdu {
	dis_pdu_header hdr;
	dis_entity_id receiver_id;
	dis_entity_id supplier_id;
	u_char num_supply_types;
	u_char pad[3];
	dis_supply_quantity *supplies;
};
typedef struct dis_resupply_offer_pdu dis_resupply_offer_pdu;
bool_t xdr_dis_resupply_offer_pdu (XDR *, dis_resupply_offer_pdu *);


struct dis_resupply_received_pdu {
	dis_pdu_header hdr;
	dis_entity_id receiver_id;
	dis_entity_id supplier_id;
	u_char num_supply_types;
	u_char pad[3];
	dis_supply_quantity *supplies;
};
typedef struct dis_resupply_received_pdu dis_resupply_received_pdu;
bool_t xdr_dis_resupply_received_pdu (XDR *, dis_resupply_received_pdu *);


struct dis_resupply_cancel_pdu {
	dis_pdu_header hdr;
	dis_entity_id receiver_id;
	dis_entity_id supplier_id;
};
typedef struct dis_resupply_cancel_pdu dis_resupply_cancel_pdu;
bool_t xdr_dis_resupply_cancel_pdu (XDR *, dis_resupply_cancel_pdu *);


struct dis_repair_complete_pdu {
	dis_pdu_header hdr;
	dis_entity_id receiver_id;
	dis_entity_id supplier_id;
	dis_repair_type repair;
	u_short pad;
};
typedef struct dis_repair_complete_pdu dis_repair_complete_pdu;
bool_t xdr_dis_repair_complete_pdu (XDR *, dis_repair_complete_pdu *);


struct dis_repair_response_pdu {
	dis_pdu_header hdr;
	dis_entity_id receiver_id;
	dis_entity_id supplier_id;
	dis_repair_result result;
	u_char pad[3];
};
typedef struct dis_repair_response_pdu dis_repair_response_pdu;
bool_t xdr_dis_repair_response_pdu (XDR *, dis_repair_response_pdu *);


typedef u_long dis_request_id;
bool_t xdr_dis_request_id (XDR *, dis_request_id *);


struct dis_create_entity_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_request_id request_id;
};
typedef struct dis_create_entity_pdu dis_create_entity_pdu;
bool_t xdr_dis_create_entity_pdu (XDR *, dis_create_entity_pdu *);


struct dis_remove_entity_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_request_id request_id;
};
typedef struct dis_remove_entity_pdu dis_remove_entity_pdu;
bool_t xdr_dis_remove_entity_pdu (XDR *, dis_remove_entity_pdu *);


struct dis_start_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_time real_time;
	dis_time sim_time;
	dis_request_id request_id;
};
typedef struct dis_start_pdu dis_start_pdu;
bool_t xdr_dis_start_pdu (XDR *, dis_start_pdu *);


struct dis_stop_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_time real_time;
	u_char reason;
	u_char behavior;
	u_char pad[2];
	dis_request_id request_id;
};
typedef struct dis_stop_pdu dis_stop_pdu;
bool_t xdr_dis_stop_pdu (XDR *, dis_stop_pdu *);


struct dis_acknowledge_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	u_short acknowledge_flag;
	u_short resp_flag;
	dis_request_id request_id;
};
typedef struct dis_acknowledge_pdu dis_acknowledge_pdu;
bool_t xdr_dis_acknowledge_pdu (XDR *, dis_acknowledge_pdu *);


struct dis_data_query_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_time interval;
	dis_request_id request_id;
	u_long num_fixed_data;
	u_long num_variable_data;
	u_long *fixed_datum_id;
	u_long *variable_datum_id;
};
typedef struct dis_data_query_pdu dis_data_query_pdu;
bool_t xdr_dis_data_query_pdu (XDR *, dis_data_query_pdu *);


struct dis_datum_spec_record {
	u_long num_fixed_data;
	u_long num_variable_data;
	dis_fixed_datum *fixed_datum;
	dis_variable_datum *variable_datum;
};
typedef struct dis_datum_spec_record dis_datum_spec_record;
bool_t xdr_dis_datum_spec_record (XDR *, dis_datum_spec_record *);


struct dis_set_data_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_request_id request_id;
	dis_datum_spec_record datum_info;
};
typedef struct dis_set_data_pdu dis_set_data_pdu;
bool_t xdr_dis_set_data_pdu (XDR *, dis_set_data_pdu *);


struct dis_data_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_request_id request_id;
	dis_datum_spec_record datum_info;
};
typedef struct dis_data_pdu dis_data_pdu;
bool_t xdr_dis_data_pdu (XDR *, dis_data_pdu *);


struct dis_event_report_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	dis_event_id event_type;
	dis_datum_spec_record datum_info;
};
typedef struct dis_event_report_pdu dis_event_report_pdu;
bool_t xdr_dis_event_report_pdu (XDR *, dis_event_report_pdu *);


struct dis_message_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	u_long pad;
	u_long num_variable_data;
	dis_variable_datum *variable_datum;
};
typedef struct dis_message_pdu dis_message_pdu;
bool_t xdr_dis_message_pdu (XDR *, dis_message_pdu *);


struct dis_track_info {
	dis_entity_id target;
	u_char emitter_id;
	u_char beam_id;
};
typedef struct dis_track_info dis_track_info;
bool_t xdr_dis_track_info (XDR *, dis_track_info *);


struct dis_beam_info {
	u_char beam_data_length;
	u_char beam_id;
	u_short beam_parm_index;
	dis_fundamental_parameters fundamental;
	u_char beam_function;
	u_char num_targets;
	u_char high_density_track_jam;
	u_char pad;
	u_long jamming_mode;
	dis_track_info *tracked_target;
};
typedef struct dis_beam_info dis_beam_info;
bool_t xdr_dis_beam_info (XDR *, dis_beam_info *);


struct dis_em_system_info {
	u_char sys_data_length;
	u_char num_beams;
	u_short pad;
	dis_emitter_system emitter_system;
	dis_entity_coord_vector location;
	dis_beam_info *beam;
};
typedef struct dis_em_system_info dis_em_system_info;
bool_t xdr_dis_em_system_info (XDR *, dis_em_system_info *);


struct dis_em_emission_pdu {
	dis_pdu_header hdr;
	dis_entity_id emitter_id;
	dis_event_id event;
	u_char state_update;
	u_char num_systems;
	u_short pad;
	dis_em_system_info *system;
};
typedef struct dis_em_emission_pdu dis_em_emission_pdu;
bool_t xdr_dis_em_emission_pdu (XDR *, dis_em_emission_pdu *);


struct dis_designator_pdu {
	dis_pdu_header hdr;
	dis_entity_id designating_id;
	u_short code_name;
	dis_entity_id designated_id;
	u_char pad;
	u_char code;
	float power;
	float wavelength;
	dis_entity_coord_vector spot_rel;
	dis_world_coordinates spot_pos;
};
typedef struct dis_designator_pdu dis_designator_pdu;
bool_t xdr_dis_designator_pdu (XDR *, dis_designator_pdu *);


struct dis_experimental_request_control_pdu {
	dis_pdu_header hdr;
	dis_entity_id requesting_id;
	dis_entity_id control_target_id;
};
typedef struct dis_experimental_request_control_pdu dis_experimental_request_control_pdu;
bool_t xdr_dis_experimental_request_control_pdu (XDR *, dis_experimental_request_control_pdu *);


struct dis_experimental_grant_control_pdu {
	dis_pdu_header hdr;
	dis_entity_id granting_id;
	dis_entity_id control_target_id;
};
typedef struct dis_experimental_grant_control_pdu dis_experimental_grant_control_pdu;
bool_t xdr_dis_experimental_grant_control_pdu (XDR *, dis_experimental_grant_control_pdu *);


struct dis_transfer_control_pdu {
	dis_pdu_header hdr;
	dis_entity_id orig_id;
	dis_entity_id recv_id;
	u_long request_id;
	u_char reliability_service;
	u_char transfer_type;
	dis_entity_id target_id;
	u_long num_record_sets;
};
typedef struct dis_transfer_control_pdu dis_transfer_control_pdu;
bool_t xdr_dis_transfer_control_pdu (XDR *, dis_transfer_control_pdu *);

typedef union {
 dis_pdu_header hdr;
 dis_entity_state_pdu entity_state;
 dis_collision_pdu collision;

 dis_fire_pdu fire;
 dis_detonation_pdu detonation;

 dis_create_entity_pdu create_entity;
 dis_remove_entity_pdu remove_entity;
 dis_start_pdu start;
 dis_stop_pdu stop;
 dis_acknowledge_pdu acknowledge;
 dis_data_query_pdu data_query;
 dis_set_data_pdu set_data;
 dis_data_pdu data;
 dis_event_report_pdu event_report;
 dis_message_pdu message;

 dis_em_emission_pdu em_emission;
 dis_designator_pdu designator;
 dis_transfer_control_pdu transfer_control;
 dis_experimental_request_control_pdu request_control;
 dis_experimental_grant_control_pdu grant_control;
 } dis_pdu;
