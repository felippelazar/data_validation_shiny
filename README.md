# data_validation_shiny

Run this app locally on Windows using Docker Desktop with **Linux containers**.

Build:

```bash
docker build -t data-validation-shiny:local .
```

Run:

```bash
docker run --rm -p 3838:3838 data-validation-shiny:local
```

Open:

`http://localhost:3838/app`
