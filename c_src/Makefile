
PROJECT_NIF_NAME=epqueue_nif

include nif.mk

ifeq ($(UNAME_SYS), darwin)
	LDFLAGS +=  -Wl,-U,_enif_alloc \
				-Wl,-U,_enif_free \
				-Wl,-U,_enif_alloc_resource \
				-Wl,-U,_enif_open_resource_type \
				-Wl,-U,_enif_release_resource \
				-Wl,-U,_enif_priv_data \
				-Wl,-U,_enif_release_binary \
				-Wl,-U,_enif_self \
				-Wl,-U,_enif_is_identical \
				-Wl,-U,_enif_is_list \
				-Wl,-U,_enif_get_int \
				-Wl,-U,_enif_get_long \
				-Wl,-U,_enif_get_list_cell \
				-Wl,-U,_enif_get_resource \
				-Wl,-U,_enif_get_tuple \
				-Wl,-U,_enif_make_atom \
				-Wl,-U,_enif_make_badarg \
				-Wl,-U,_enif_make_existing_atom \
				-Wl,-U,_enif_make_new_binary \
				-Wl,-U,_enif_make_resource \
				-Wl,-U,_enif_make_tuple \
				-Wl,-U,_enif_make_int \
                -Wl,-U,_enif_make_long \
				-Wl,-U,_enif_binary_to_term \
				-Wl,-U,_enif_term_to_binary \
				-Wl,-U,_enif_mutex_create \
				-Wl,-U,_enif_mutex_destroy \
				-Wl,-U,_enif_mutex_lock \
				-Wl,-U,_enif_mutex_unlock
endif

CXXFLAGS+=  -g -Wextra -Werror -Wno-missing-field-initializers -fno-rtti -fno-exceptions
LDFLAGS +=  -lstdc++