#pragma once

class IRSource {
 public:
  virtual ~IRSource() = default;

  virtual bool isExecuting() const = 0;
};

class TracedIRSource : public IRSource {
 public:
  virtual bool isExecuting() const override { return true; }
};
