#pragma once

class IRSource {
 public:
  virtual bool isExecuting() const = 0;
};

class TracedIRSource : public IRSource {
 public:
  virtual bool isExecuting() const override { return true; }
};

class SavedIRSource : public IRSource {
 public:
  virtual bool isExecuting() const override { return false; }
};
