type compression_method =
    Stored                     (** data is stored without compression *)
  | Deflated                   (** data is compressed with the ``deflate'' algorithm *)

type entry =
  { filename: string;          (** file name for entry *)
    extra: string;             (** extra information attached to entry *)
    comment: string;           (** comment attached to entry *)
    methd: compression_method; (** compression method *)
    mtime: float;              (** last modification time (seconds since epoch) *)
    crc: int32;                (** cyclic redundancy check for data *)
    uncompressed_size: int;    (** size of original data in bytes *)
    compressed_size: int;      (** size of compressed data *)
    is_directory: bool;        (** whether this entry represents a directory *)
    file_offset: int64         (** for internal use *)
  }

type in_file =
  { if_filename: string;
    mutable if_channel: Pervasives.in_channel;
    if_entries: entry list;
    if_directory: (string, entry) Hashtbl.t;
    if_comment: string }

val open_in: string -> in_file
val entries: in_file -> entry list

val set_if_channel: in_file -> string -> unit

val find_entry: in_file -> string -> entry
val read_entry: in_file -> entry -> string

val close_in: in_file -> unit
exception Error of string * string * string
