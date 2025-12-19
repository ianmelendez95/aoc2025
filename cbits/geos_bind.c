#include <stdlib.h>
#include <stdarg.h>
#include <stdio.h>

#include <geos_c.h>

static void geos_message_handler(const char* fmt, ...)
{
    va_list ap;
    va_start(ap, fmt);
    vprintf (fmt, ap);
    va_end(ap);
}

char hgeos_contains(char* wkt_a, char* wkt_b) {
  char contains;

  /* Send notice and error messages to our stdout handler */
  initGEOS(geos_message_handler, geos_message_handler);

  /* Read the WKT into geometry objects */
  GEOSWKTReader* reader = GEOSWKTReader_create();
  GEOSGeometry* geom_a = GEOSWKTReader_read(reader, wkt_a);
  GEOSGeometry* geom_b = GEOSWKTReader_read(reader, wkt_b);

  /* Calculate the intersection */
  contains = GEOSContains(geom_a, geom_b);

  /* Print answer */
  // printf("Geometry A:         %s\n", wkt_a);
  // printf("Geometry B:         %s\n", wkt_b);
  // printf("Contains(A, B): %s\n", contains ? "true" : "false");

  /* Clean up everything we allocated */
  GEOSWKTReader_destroy(reader);
  GEOSGeom_destroy(geom_a);
  GEOSGeom_destroy(geom_b);

  /* Clean up the global context */
  finishGEOS();

  return contains;
}

