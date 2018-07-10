-module(mafiapp).
-record(mafiapp_friends, {name,
                            contact=[],
                            info=[],
                            expertise}).
-record(mafiapp_services, {from,
                            to,
                            date,
                            description}). 

