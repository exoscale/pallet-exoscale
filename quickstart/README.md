A quickstart project to show how getting up and running works:

First, put your credentials in `$HOME/.pallet/services/exoscale.clj`,
as follows:

```clojure
{:exoscale {:provider "exoscale",
            :api-key "<EXOSCALE_API_KEY>",
            :api-secret "<EXOSCALE_API_SECRET>",
            :endpoint "https://run1.exoscale.ch/client/api"}}
```

Then just `lein run` or `lein run <n>` where `n` is the amount
of instances you want to launch.
The instances all run nginx with a default text.
